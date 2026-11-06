// Development partnerships on the Country page (2026-11-03; src/country/devpart.ts,
// views/profile/DevelopmentPartnerships.tsx): the file contract, the decoding (list fields, missing aid kept
// missing), and the section in both perspectives on the exported files when they are present (Australia, a
// donor; Ukraine, a recipient), with a stub profile context.

import { existsSync, readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { afterEach, describe, expect, it } from 'vitest';
import { cleanup, fireEvent, render, screen, waitFor, within } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { CountryStore, setCountryStoreForTests, type CountrySource } from '../data/store.ts';
import { aidWords, byRoute, checkDevpartDoc, compactRoleWords, constraintWords, devpartPairs, devpartRows, tieWords, type DevpartDoc, type DevpartPair } from '../devpart.ts';
import { ProfileProvider, type ProfileCtx } from '../views/profile/common.tsx';
import { DEVPART_QUESTION, DevelopmentPartnershipsSection, DevelopmentPartnershipsSummary, NO_DEVELOPMENT_SOLUTION } from '../views/profile/DevelopmentPartnerships.tsx';

const DATA = resolve(__dirname, '../../../public/data/country');
const manifest = existsSync(resolve(DATA, 'manifest.json')) ? (JSON.parse(readFileSync(resolve(DATA, 'manifest.json'), 'utf8')) as { countries: { iso3: string; name: string; devpart?: string | null; status: string; file: string | null }[] }) : null;
const entryOf = (iso: string) => manifest?.countries.find((c) => c.iso3 === iso) ?? null;
const haveReal = (iso: string) => !!entryOf(iso)?.devpart && existsSync(resolve(DATA, entryOf(iso)!.devpart!));

const WORDS = {
  signal: { partner: 'Partner: sources from trusted partners', aib_shortfall: 'allied supply falls short' }, signal_short: { partner: 'Partner', aib_shortfall: 'allied supply falls short' },
  anchor: { direct_supply_share: 'a share of world supply' }, support: { feasibility: 'feasibility' },
  route: { development: 'Development partnership', commercial: 'Commercial route (not aid)', review: 'Review / insufficient evidence' },
  aid_status: { recorded: 'aid recorded at this step', other_stage: 'aid recorded at other steps only', none: 'no aid recorded for the pair', not_reporting: 'the donor does not report purpose-coded aid to the CRS' },
  aid_gap: { aligned: 'Existing aid already at the step', no_aib: 'Aid with no partnership in this analysis' }, constraint: {}, benefit_donor: {}, benefit_recipient: {}, fail: {},
};
function synthetic(over: Partial<DevpartDoc> = {}): DevpartDoc {
  return {
    schema: 'opsi.country.devpart', schema_version: 1, iso3: 'TST', country: 'Testland', perspective: 'donor', perspective_reason: null,
    meta: { created: 't', as_of_label: '30 October 2026', aid_years: [2020, 2024], aid_flow: 'disbursement', dip_years: [2020, 2024], need_bar: 0.67, sources: ['OECD CRS'] },
    words: WORDS,
    steps: { key: ['wind-assembly'], tech: ['Wind'], stage: ['Assembly'], step_name: ['wind turbines'], choice: ['partner'], fence: ['medium'], need: [0.85], conf: ['high'],
             signals: ['partner;aib_shortfall'], donor_tier: [1], aib_status: ['gap'], has_development: [true], n_considered: [2], fails: [null] },
    pairs: { donor_iso3: ['TST', 'TST'], recipient_iso3: ['AAA', 'BBB'], key: ['wind-assembly', 'wind-assembly'], route: ['development', 'commercial'], rationale: ['r', 'r'],
             oda_status: ['not_reporting', 'none'], current_oda: [null, 0], donor_signals: ['partner;aib_shortfall', 'partner'], catch_up_anchor_type: ['direct_supply_share', null] },
    aid: { reporting: false, gap: {}, top: {}, no_aib: {} },
    ...over,
  };
}

describe('the relationship words', () => {
  const base = devpartPairs(synthetic())[0];
  const w = { ...WORDS, compact: { 'SC-WIND': 'Wind & Hydropower Equipment' }, unobserved: { input_costs: 'input costs not covered by the data' }, constraint: { no_capacity: 'no operating or committed plant' } };
  it('tells an observed zero from data that do not cover the pair', () => {
    expect(tieWords({ ...base, imf_dip_tie: null, cm_fdi_projects: null, oci_finance: null, aib_pairing: false, recipient_compact_role: null })).toBe('not covered by the data');
    expect(tieWords({ ...base, imf_dip_tie: 0, cm_fdi_projects: null, oci_finance: null, aib_pairing: false, recipient_compact_role: null })).toBe('no direct investment position recorded');
    expect(tieWords({ ...base, imf_dip_tie: 7.75, imf_dip_basis: 'derived', cm_fdi_projects: null, oci_finance: null, aib_pairing: true, recipient_compact_role: null })).toBe('direct investment $7.8 million (economy-wide, partner-reported); a simulated AIB investment pairing (a model result, not an existing tie)');
  });
  it('names compacts, and only the compact of the step', () => {
    expect(compactRoleWords('SC-WIND: Specialized technology supplier;SC-MAGNETS: Resource supplier', w, ['SC-WIND'])).toEqual(['Specialized technology supplier in the Wind & Hydropower Equipment compact']);
    expect(constraintWords(['no_capacity'], ['input_costs'], w)).toBe('no operating or committed plant; input costs not covered by the data');
  });
  it('says where the aid money is, not the most specific code present', () => {
    const p = { ...base, oda_status: 'recorded', current_oda: 0.6, oda_named: 0.01, oda_specificity: 'stage' } as DevpartPair;
    expect(aidWords(p, WORDS)).toBe('$0.6 million at this step (stage-wide, technology not identified; <$0.1 million coded to the technology)');
  });
});

describe('the development-partnership file', () => {
  it('passes the contract and decodes its list fields', () => {
    const d = synthetic();
    expect(checkDevpartDoc(d, 'TST')).toEqual([]);
    const p = devpartPairs(d);
    expect(p).toHaveLength(2);
    expect(p[0].donor_signals).toEqual(['partner', 'aib_shortfall']);
    expect(p[1].catch_up_anchor_type).toEqual([]);
    expect(byRoute(p).development.map((x) => x.recipient_iso3)).toEqual(['AAA']);
  });
  it('rejects a wrong economy, an unknown route and ragged columns', () => {
    expect(checkDevpartDoc(synthetic(), 'XXX').join(' ')).toMatch(/is not XXX/);
    expect(checkDevpartDoc(synthetic({ pairs: { ...synthetic().pairs!, route: ['development', 'grant'] } })).join(' ')).toMatch(/unknown route/);
    expect(checkDevpartDoc(synthetic({ pairs: { ...synthetic().pairs!, key: ['wind-assembly'] } })).join(' ')).toMatch(/different lengths/);
  });
  it('keeps missing aid missing in words, never "$0"', () => {
    const p = devpartPairs(synthetic());
    expect(aidWords(p[0] as DevpartPair, WORDS)).toBe('the donor does not report purpose-coded aid to the CRS');
    expect(aidWords(p[1] as DevpartPair, WORDS)).toBe('no aid recorded for the pair');
    expect(aidWords(p[0] as DevpartPair, WORDS)).not.toMatch(/\$0/);
  });
  it('reads columns with nulls as rows', () => {
    expect(devpartRows<{ a: number | null; fails: string[] }>({ a: [1, null], fails: ['x;y', null] })).toEqual([{ a: 1, fails: ['x', 'y'] }, { a: null, fails: [] }]);
  });
});

// ---- the section, on the exported files --------------------------------------------------------------------

function source(files: Record<string, unknown>): CountrySource {
  return {
    manifest: async () => manifest,
    json: async (f) => {
      if (f in files) return files[f];
      return JSON.parse(readFileSync(resolve(DATA, f), 'utf8'));
    },
    url: (f) => `/data/country/${f}`,
  };
}
function renderSection(iso: string, files: Record<string, unknown> = {}, entryOver: Record<string, unknown> = {}) {
  setCountryStoreForTests(new CountryStore(source(files)));
  const e = { ...entryOf(iso), ...entryOver };
  const ctx = {
    sheet: { iso3: iso, meta: { country: e.name ?? iso } }, entry: e, manifest: manifest ?? { countries: [] }, state: {}, cell: null, partner: null,
    setCell: () => undefined, setPartner: () => undefined, setAll: () => undefined, driversDoc: null, driversStatus: 'idle', retryDrivers: () => undefined,
    fileUrl: (f: string) => f,
  } as unknown as ProfileCtx;
  return render(
    <MemoryRouter>
      <ProfileProvider value={ctx}>
        <DevelopmentPartnershipsSummary />
        <DevelopmentPartnershipsSection />
      </ProfileProvider>
    </MemoryRouter>,
  );
}

afterEach(() => {
  cleanup();
  setCountryStoreForTests(null);
});

describe('the Development partnerships section', () => {
  it.skipIf(!haveReal('AUS'))('reads as the donor view for Australia, without a score', async () => {
    renderSection('AUS');
    const sec = await screen.findByTestId('devpart-donor', {}, { timeout: 10_000 });
    expect(screen.getByTestId('devpart-question').textContent).toBe(DEVPART_QUESTION.donor('Australia'));
    const table = within(sec).getByTestId('devpart-table-development');
    // the reasons are columns: no score column anywhere
    const heads = within(table).getAllByRole('columnheader').map((h) => h.textContent ?? '');
    expect(heads.some((h) => /Catch Up anchor/.test(h))).toBe(true);
    expect(heads.some((h) => /Existing aid relationship/.test(h))).toBe(true);
    expect(heads.some((h) => /score/i.test(h))).toBe(false);
    // rare-earth refining: no developing partner passes; the commercial route lists Malaysia and Thailand
    expect(within(sec).getByTestId('devpart-none').textContent).toContain('rare-earth and battery-metal refining');
    fireEvent.click(within(sec).getByRole('button', { name: /^Commercial route/ }));
    await waitFor(() => expect(within(sec).getByTestId('devpart-table-commercial')).toBeTruthy());
    // the current-aid comparison never calls aid outside the objective wasteful
    expect(within(sec).getByTestId('devpart-aid-gap').textContent).toMatch(/aid outside it is not judged/);
    expect(screen.getByTestId('devpart-summary-card').textContent).toMatch(/developing partners where Australia's need meets a credible Catch Up pathway/);
  }, 30_000);

  it.skipIf(!haveReal('UKR'))('flips to the recipient view for Ukraine', async () => {
    renderSection('UKR');
    const sec = await screen.findByTestId('devpart-recipient', {}, { timeout: 10_000 });
    expect(screen.getByTestId('devpart-question').textContent).toBe(DEVPART_QUESTION.recipient('Ukraine'));
    expect(within(sec).getByTestId('devpart-recipient-steps').textContent).toContain('EV drivetrain components');
    const partners = within(sec).getByTestId('devpart-recipient-partners');
    expect(within(partners).getAllByRole('columnheader').map((h) => h.textContent ?? '').some((h) => /What Ukraine builds/.test(h))).toBe(true);
    expect(partners.textContent).toContain('Australia');
  }, 30_000);

  it('says so when no partner qualifies, and when the economy has no perspective', async () => {
    const none = synthetic({ perspective: 'none', perspective_reason: 'high-income economy outside the Allied Industrial Base demand scope', steps: undefined, pairs: undefined, aid: undefined });
    renderSection('TST', { 'devpart-TST.0000aaaa.json': none }, { iso3: 'TST', name: 'Testland', devpart: 'devpart-TST.0000aaaa.json' });
    expect((await screen.findByTestId('devpart-none-perspective', {}, { timeout: 10_000 })).textContent).toMatch(/outside the Allied Industrial Base demand scope/);
    cleanup();
    const empty = synthetic({ pairs: { ...synthetic().pairs!, route: ['commercial', 'commercial'] } });
    renderSection('TST', { 'devpart-TST.0000bbbb.json': empty }, { iso3: 'TST', name: 'Testland', devpart: 'devpart-TST.0000bbbb.json' });
    expect((await screen.findByTestId('devpart-summary', {}, { timeout: 10_000 })).textContent).toContain(NO_DEVELOPMENT_SOLUTION);
    // the needs with no development partner stay reachable with no development pair at all
    const card = screen.getByTestId('devpart-none');
    expect(card.textContent).toContain('wind turbines');
    expect(card.textContent).toContain(NO_DEVELOPMENT_SOLUTION);
  }, 30_000);

  it('asks no donor question of an economy with no perspective', async () => {
    const none = synthetic({ perspective: 'none', perspective_reason: 'income group unknown', steps: undefined, pairs: undefined, aid: undefined });
    renderSection('TST', { 'devpart-TST.0000cccc.json': none }, { iso3: 'TST', name: 'Testland', devpart: 'devpart-TST.0000cccc.json' });
    await screen.findByTestId('devpart-none-perspective', {}, { timeout: 10_000 });
    expect(screen.queryByTestId('devpart-question')).toBeNull();
  }, 30_000);

  it('shows a recipient its Preserve steps as the commercial route', async () => {
    const rec = synthetic({
      perspective: 'recipient',
      steps: { key: ['solar-refining'], tech: ['Solar'], stage: ['Refining'], step_name: ['polysilicon'], choice: ['catch_up'], anchors: [null], supports: [null], constraints: [null],
               unobserved: [null], recipient_fail: ['anchor'], recipient_ok: [false], n_donors: [0], thin_anchor: [false] },
      pairs: { donor_iso3: ['DON'], recipient_iso3: ['TST'], key: ['wind-assembly'], route: ['commercial'], rationale: ['r'], oda_status: ['none'], current_oda: [0],
               recipient_choice: ['preserve'], donor_signals: ['partner'] },
      preserve_steps: { key: ['wind-assembly'], tech: ['Wind'], stage: ['Assembly'], step_name: ['wind turbines'], conf: ['high'], ev: ['high'], preserve_fail: [null], n_commercial: [1], n_review: [0] },
      aid: undefined,
    });
    renderSection('TST', { 'devpart-TST.0000dddd.json': rec }, { iso3: 'TST', name: 'Testland', devpart: 'devpart-TST.0000dddd.json' });
    const card = await screen.findByTestId('devpart-recipient-commercial', {}, { timeout: 10_000 });
    expect(card.textContent).toContain('wind turbines');
  }, 30_000);
});
