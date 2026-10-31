// S5 of the Country profile, "Partnerships" (spec_country.md A4.2; IA refactor 2026-10-16, brief s.12D; the
// three lenses 2026-10-19, aib_scenarios_spec.md s.5 C and s.6): the shared three-lens component
// (src/partnerships) with the economy as its focus. Four kinds of partnership (URL `ptab`: trade,
// investment, development, geopolitical), each read three ways, a cross-lens summary first:
//   Today           Trade: its largest trade partners (fact sheet tie order: average import and export
//                   shares) and, in a disclosure, who supplies it in each sector. Investment: its IMF
//                   direct-investment positions and factory projects (ties chunk). Development: official
//                   aid it receives and gives (OECD; the Sector bundle, loaded only on that tab).
//                   Geopolitical: its coalition tier, UN-vote bloc and agreements, and the economies it
//                   shares an agreement with.
//   OSI ranking  the partner scores of the fact sheet at the NEIS baseline (Partnership Opportunity,
//                   Friendshore, Development Potential; sector by sector in disclosures), and the allied
//                   coalition (the Country manifest's tiers) with Friendshore's alignment part.
//   AIB model       the toggled scenario (scn: Decoupling, or Managed Interdependence from its file; the
//                   toggle sits in the panel): its coordinated trade, the investment flows where it invests
//                   or hosts, the BUILD nodes that concern it, its compact coalitions.
// The partner-by-partner table (priority partners beside today's partners) stays in a disclosure after the
// lenses. Since 2026-10-30 (the fences) the section is back on the page as it read before the 2026-10-29
// redesign, open, as the Medium fence's Partner sub-section (id medium-partner, heading level 3, the old
// #partnerships, #partners and #allied-partnerships anchors as aliases) under a lead naming the economy's
// Partner nodes; the model's partner scores under the reader's Perspective (the dials) stay in Legacy OSI
// (PartnerScoresFold).
//
// Above the lenses (2026-10-20, strategic task spec s.19): the relationship thesis of the economy and its
// selected partner (URL `partner`, the profile's own; the picker lists the fact sheet's partners, those the
// model writes a thesis with first), from the strategic layer of the AIB document (src/partnerships/
// RelationshipThesis.tsx; shown only when the bundle has the layer). The cross-lens summary says where
// that partner sits in the three lenses.
//
// The NEIS copy refresh (2026-10-30): as the Partner sub-section it opens with what Partner means for the
// economy (the guide's "Where should this country rely on allies?" intro), then the economy's Partner steps,
// then "Which partners are best placed to help?" over the thesis and the lenses; each partner score card
// says what the score is for (Friendshore, Partnership Opportunity, Development Potential); the AIB cards say
// "remaining gap", "Build capacity" and "country that could add capacity" (vocabulary TERMS, AIB_CLASS_NAME).

import { lazy, Suspense, useCallback, useEffect, useMemo, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { Term } from '../../../components/Term.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Loading, Select } from '../../../components/ui.tsx';
import { AIB_CLASS_NAME, PARTNERSHIP, SCENARIO_NAME } from '../../../content/vocabulary.ts';
import { cellOf, type PartnerRow } from '../../contract.ts';
import { PartnerSlope } from '../../charts/PartnerSlope.tsx';
import { SupplierMatrix } from '../../charts/SupplierMatrix.tsx';
import { strategicOf, type AibDoc, type AibInvestmentFlows, type AibManagedScenarioDoc } from '../../../aib/contract.ts';
import { ClassChip } from '../../../aib/charts/ClassChip.tsx';
import { useAibManifest, useAibScenario, useAibScenarioDoc, type ScenarioLoad } from '../../../aib/data/hooks.ts';
import { aibStore } from '../../../aib/data/store.ts';
import { compactTitle, fmtPp, fmtShare, fmtUsd, listWords, plural } from '../../../aib/derive/format.ts';
import { hostRanksOf, investmentPairs, partnerTotals, rankWords, type PartnerTotal } from '../../../aib/derive/investment.ts';
import { lensCoalitions, lensNodes, NO_FILTER, partnersInPairs, scenarioCompacts, type LensScenario, type ScenarioCompactRel } from '../../../aib/derive/partnerships.ts';
import { effectiveScenario, hasScenarios, scenarioLabel, scenarioNameOf, scenarioShockLabel } from '../../../aib/derive/scenarios.ts';
import { compactHref, strategicCompactHref, withScenario } from '../../../aib/nav.ts';
import { ScenarioToggle } from '../../../aib/ScenarioToggle.tsx';
import { INVEST_COLOR, ModelledNote } from '../../../aib/views/networkParts.tsx';
import { AGGREGATE_NAMES, agreementNames, agreementTies, alignmentWords, coalitionOf, dipPartners, tierWords } from '../../../partnerships/derive.ts';
import { kindOf, lensNames, lensNone, LENS_LOADING, type LensId, type LensNames, type PartnershipKind } from '../../../partnerships/lenses.ts';
import { PartnershipLenses, RankedNames, type LensPanelSpec } from '../../../partnerships/PartnershipLenses.tsx';
import { RelationshipThesis } from '../../../partnerships/RelationshipThesis.tsx';
import { thesisPartners } from '../../../partnerships/thesis.ts';
import PartnersView from '../../../views/Partners/PartnersView.tsx';
import { loadTies, tiesName, type Ties } from '../../../views/Partners/ties.ts';
import { agreementRows, aibTradeOf, buildNodesFor, tradeTiesToday } from '../../derive/framing.ts';
import { devDataLink } from '../../derive/links.ts';
import { GROUP_WORD, partnerTableRows, slopeModel, supplierModel } from '../../derive/model.ts';
import { agreementsText, alignmentText, matchText, partnerReason, proseName, poss, cap, tierPhrase } from '../../derive/text.ts';
import { csvWithSource, Fold, LinkRow, Section, useProfile } from './common.tsx';
import { PartnersBySector } from './PartnersBySector.tsx';

const OdaToday = lazy(() => import('../../../partnerships/OdaToday.tsx'));

type Row = PartnerRow & { extra: boolean };

/**
 * What each Legacy OSI partner score is for, seen from the country (the guide's Country-page definitions;
 * candidates for the shared vocabulary beside PARTNERSHIP.friendshore / opportunity / development).
 */
export const COUNTRY_PARTNER_SCORE_WORDS = {
  fs: 'Where the country could shift sourcing or investment to reduce a strategic dependency.',
  po: "Where the partner is an attractive market for the country's competitive sectors.",
  dp: 'Where joint investment could help build a new supply base and create value in a developing partner economy.',
} as const;

/** The Partner sub-section's opening (guide: "Where should this country rely on allies?"). */
export const PARTNER_SUBSECTION_INTRO =
  'These are capabilities the country needs but should not necessarily reproduce at home. The question is which partners have the right capability, how concentrated the alternative supply is, and what trade, investment or agreements would make that reliance durable.';

/** The partners worth showing as a short list: ranked rows by one score, best first. */
function topBy(rows: readonly PartnerRow[], k: 'po' | 'fs' | 'dp', n = 5): PartnerRow[] {
  return rows.filter((r) => r[k].v !== null).sort((a, b) => (b[k].v ?? 0) - (a[k].v ?? 0)).slice(0, n);
}

/** A card inside a lens panel (an h4: the panels' headings are h3). */
function Card({ id, title, children }: { id: string; title: string; children: ReactNode }) {
  return (
    <section className="card p-5" aria-labelledby={`${id}-h`} data-testid={id}>
      <h4 id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {title}
      </h4>
      <div className="mt-2 space-y-2 text-sm text-paper-dim">{children}</div>
    </section>
  );
}

/** A partner's name: a link to its Country profile when it has one (the AIB scenario toggle carried). */
export function PartnerName({ iso3, name }: { iso3: string; name: string }) {
  const { manifest } = useProfile();
  const { href } = useAibScenario();
  const has = manifest.countries.some((e) => e.iso3 === iso3 && e.status !== 'no_data' && !!e.file);
  return has ? (
    <Link className="link text-paper" to={href(`/countries/${iso3}`)}>
      {name}
    </Link>
  ) : (
    <span className="text-paper">{name}</span>
  );
}

function ScoreList({ rows, k, label }: { rows: PartnerRow[]; k: 'po' | 'fs' | 'dp'; label: string }) {
  const { sheet } = useProfile();
  if (!rows.length) return <p className="text-paper-mute">No partner has a {label} score for {proseName(sheet.meta.country)}.</p>;
  return (
    <ol className="space-y-1.5">
      {rows.map((r, i) => (
        <li key={r.iso3} className="flex items-baseline justify-between gap-3">
          <span>
            <span className="tabular text-paper-mute">{i + 1}. </span>
            <PartnerName iso3={r.iso3} name={r.name} />
            {r.income_group && k === 'dp' && <span className="text-xs text-paper-mute"> · {r.income_group}</span>}
          </span>
          <span className="tabular text-xs text-paper-mute">{r[k].d}</span>
        </li>
      ))}
    </ol>
  );
}

/** The ties chunk of the Strategic bundle (one load per page; the loader caches it). */
function useTies(): { ties: Ties | null; error: string | null } | null {
  const [s, set] = useState<{ ties: Ties | null; error: string | null } | null>(null);
  useEffect(() => {
    let live = true;
    loadTies().then(
      (t) => live && set({ ties: t, error: t ? null : 'This data build has no partner-by-sector data.' }),
      (e: Error) => live && set({ ties: null, error: `Could not load the partner data: ${e.message}` }),
    );
    return () => {
      live = false;
    };
  }, []);
  return s;
}

/** The AIB model lens's scenario on the Country profile: the URL's scn, the Managed Interdependence file, the toggle. */
interface CountryScenario {
  doc: AibDoc | null;
  available: boolean;
  id: LensScenario;
  label: string;
  load: ScenarioLoad<AibManagedScenarioDoc>;
  /** the scenario's compacts (null while the Managed Interdependence file is not there) */
  compacts: ScenarioCompactRel[] | null;
  flows: AibInvestmentFlows | null;
  tools: ReactNode;
  /** an economy's name: the AIB document's, the scenario file's, the fact sheet's partners' */
  name: (iso: string) => string;
}

function useCountryScenario(): CountryScenario {
  const { sheet, aib, manifest } = useProfile();
  const doc = aib?.doc ?? null;
  const toggle = useAibScenario();
  const m = useAibManifest();
  const available = doc ? hasScenarios(doc) : false;
  const id = effectiveScenario(toggle.scenario, available);
  const load = useAibScenarioDoc(id === 'managed' ? 'managed' : null) as ScenarioLoad<AibManagedScenarioDoc>;
  const compacts = useMemo(() => (doc ? scenarioCompacts(doc, id, load.doc) : null), [doc, id, load.doc]);
  const flows = !doc ? null : id === 'decoupling' ? (doc.investmentFlows ?? null) : (load.doc?.investmentFlows ?? null);
  const prefetch = useCallback(() => aibStore().prefetchScenario(m.data, 'managed'), [m.data]);
  const names = aib?.names;
  const name = useMemo(() => {
    const rows = new Map<string, string>(Object.entries(AGGREGATE_NAMES));
    for (const c of manifest.countries) rows.set(c.iso3, c.name);
    for (const r of sheet.partners.rows) rows.set(r.iso3, r.name);
    // the profile's own names first (the fact sheet, the Country roster), then the AIB document's
    const base = (iso: string | null | undefined) => (iso ? (rows.get(iso) ?? names?.[iso] ?? iso) : '');
    const f = scenarioNameOf(base, load.doc);
    return (iso: string) => f(iso);
  }, [names, sheet, manifest, load.doc]);
  return {
    doc,
    available,
    id,
    label: scenarioLabel(id, doc?.meta),
    load,
    compacts,
    flows,
    name,
    tools: available && doc ? <ScenarioToggle value={id} onChange={toggle.setScenario} meta={doc.meta} onIntent={prefetch} /> : null,
  };
}

/** Names from the ties chunk (it covers every scored economy), else the scenario namer's. */
function tiesNamer(ties: Ties | null | undefined, fallback: (iso: string) => string): (iso: string) => string {
  return (x) => {
    const n = ties ? tiesName(ties, x) : x;
    return n === x ? fallback(x) : n;
  };
}

/** The AIB model lens while its data is not there: the document loading or missing, the scenario file loading, failed or absent. */
function aibPending(sc: CountryScenario, status: 'loading' | 'ready' | 'error' | undefined): LensPanelSpec | null {
  if (!sc.doc) {
    return status === 'loading'
      ? { names: LENS_LOADING, content: <p className="text-sm text-paper-mute">Loading the Allied Industrial Base results…</p> }
      : { names: lensNone('the Allied Industrial Base results are not available in this build'), empty: 'The Allied Industrial Base results are not available in this build.' };
  }
  if (sc.id === 'decoupling' || sc.load.doc) return null;
  if (sc.load.error) {
    return {
      names: lensNone('the Managed Interdependence data could not be loaded'),
      tools: sc.tools,
      content: (
        <p className="text-sm text-paper-mute">
          The Managed Interdependence data could not be loaded.{' '}
          <button type="button" className="link" onClick={sc.load.retry}>
            Retry
          </button>
        </p>
      ),
    };
  }
  if (sc.load.missing) return { names: lensNone('this build has no Managed Interdependence detail'), tools: sc.tools, empty: `This data build has no Managed Interdependence detail for the partnerships: choose ${SCENARIO_NAME.decoupling}.` };
  return { names: LENS_LOADING, tools: sc.tools, content: <Loading label="Loading the Managed Interdependence results" /> };
}

/** The scenario's note under an AIB card. */
function ScenarioNote({ sc }: { sc: CountryScenario }) {
  if (!sc.doc) return null;
  return <ModelledNote className="mt-3">{`${sc.label}: ${scenarioShockLabel(sc.doc.meta, sc.id)}`}</ModelledNote>;
}

/**
 * "Partnerships": the kind of partnership (URL `ptab`), the cross-lens summary and the three lenses, then
 * the partner-by-partner table in a disclosure. `title`, `aliases` and `lead` (the economy's Partner steps,
 * shown after the intro and before the analysis) let a page mount it as a sub-section of its own.
 */
export function PartnershipsSection({
  id = 'partnerships', level = 2, title = 'Partnerships', aliases, lead,
}: { id?: string; level?: 2 | 3; title?: string; aliases?: readonly string[]; lead?: ReactNode } = {}) {
  const { sheet, state, setPtab } = useProfile();
  const name = proseName(sheet.meta.country);
  const kind = kindOf(state.ptab);
  const rows = sheet.partners.rows;
  const onKind = (k: PartnershipKind) => setPtab?.(k);
  const H = level === 3 ? 'h4' : 'h3';
  return (
    <Section id={id} level={level} title={title} aliases={aliases} intro={PARTNER_SUBSECTION_INTRO}>
      {lead}
      <div className="space-y-2" data-testid="partner-analysis-intro">
        <H className="text-base font-medium text-paper">{PARTNERSHIP.heading}</H>
        <p className="max-w-3xl text-sm leading-relaxed text-paper-dim">
          Who {name} could trade, invest, build and align with, read three ways: the ties it has today (observed data), what the Legacy OSI partner scores suggest (the{' '}
          <Term id="psi" />, with <Term id="fs" /> and <Term id="dp" />, weigh preferences and are shown at the NEIS defaults) and how the Allied Industrial Base analysis would divide
          the work under the chosen scenario. The summary of each kind says where the three agree and where they differ. Select a Partner step above for its best-placed partners.
          First, the strategic case for a partnership between {name} and the partner you choose.
        </p>
      </div>
      <PairThesisCard />
      <div data-testid={`ptab-${kind}`}>
        {kind === 'trade' && <TradeLenses onKind={onKind} />}
        {kind === 'investment' && <InvestmentLenses onKind={onKind} />}
        {kind === 'development' && <DevelopmentLenses onKind={onKind} />}
        {kind === 'geopolitical' && <GeopoliticalLenses onKind={onKind} />}
      </div>
      <div>
        <Fold id="partner-table" summary={`Every partner of ${name}: priority partners beside today's partners, partner by partner (scored at the NEIS default weights)`}>
          {rows.length ? (
            <>
              <SlopeCard />
              <PartnerTable />
            </>
          ) : (
            <p className="text-sm text-paper-mute">{cap(name)} has no partner rows in the Partnership Strength Index.</p>
          )}
        </Fold>
      </div>
    </Section>
  );
}

/** The model's partner scores under the reader's Perspective (the dials; Legacy OSI since 2026-10-29). */
export function PartnerScoresFold() {
  return (
    <Fold id="partners-model" engine summary="Partner scores under your perspective: Partnership Strength, Friendshore, Partnership Opportunity and Development Potential (Legacy OSI)">
      <PartnersView embedded />
    </Fold>
  );
}

/** The link to an economy's Country profile (null when it has none), the AIB scenario toggle carried. */
function useProfileHref(): (iso: string) => string | null {
  const { manifest } = useProfile();
  const { href } = useAibScenario();
  return useMemo(() => {
    const has = new Set(manifest.countries.filter((e) => e.status !== 'no_data' && !!e.file).map((e) => e.iso3));
    return (iso: string) => (has.has(iso) ? href(`/countries/${iso}`) : null);
  }, [manifest, href]);
}

/**
 * The relationship thesis of the economy and its selected partner (the profile's `partner`), from the AIB
 * document's strategic layer; nothing in a bundle without it. The picker lists the fact sheet's partners.
 */
function PairThesisCard() {
  const { sheet, aib, manifest, partner, setPartner } = useProfile();
  const { href } = useAibScenario();
  const doc = aib?.doc ?? null;
  const strategic = useMemo(() => strategicOf(doc), [doc]);
  const iso = sheet.iso3;
  const rows = sheet.partners.rows;
  const aibNames = aib?.names;
  const nameOf = useMemo(() => {
    const m = new Map<string, string>(Object.entries(AGGREGATE_NAMES));
    for (const [k, v] of Object.entries(aibNames ?? {})) m.set(k, v);
    for (const c of manifest.countries) m.set(c.iso3, c.name);
    for (const r of rows) m.set(r.iso3, r.name);
    m.set(iso, sheet.meta.country);
    return (x: string) => m.get(x) ?? x;
  }, [aibNames, manifest, rows, iso, sheet.meta.country]);
  const profiles = useMemo(() => new Set(manifest.countries.filter((e) => e.status !== 'no_data' && !!e.file).map((e) => e.iso3)), [manifest]);
  const withThesis = useMemo(() => new Set(thesisPartners(strategic, iso)), [strategic, iso]);
  if (!strategic) return null;
  const choices = rows.filter((r) => r.iso3 !== iso);
  // the fact sheet's order (Partnership Strength) within each group, the partners with a thesis first
  const ordered = [...choices.filter((r) => withThesis.has(r.iso3)), ...choices.filter((r) => !withThesis.has(r.iso3))];
  const b = partner && partner !== iso && choices.some((r) => r.iso3 === partner) ? partner : null;
  const name = proseName(sheet.meta.country);
  const picker = ordered.length ? (
    <Select
      label="Partner"
      value={b ?? ''}
      options={[
        ...(b ? [] : [{ value: '', label: 'Choose a partner' }]),
        ...ordered.map((r) => ({ value: r.iso3, label: r.name, group: withThesis.has(r.iso3) ? 'With a relationship thesis' : 'No relationship thesis' })),
      ]}
      onChange={(v) => setPartner(v || null)}
      className="max-w-xs"
    />
  ) : null;
  const pairLink = `/aib/partnerships?country=${iso}${b ? `&partner=${b}` : ''}`;
  return (
    <RelationshipThesis
      id="profile-thesis"
      strategic={strategic}
      a={iso}
      b={b}
      name={nameOf}
      compactHref={(id) => href(strategicCompactHref(id))}
      countryHref={(x) => (x !== iso && profiles.has(x) ? href(`/countries/${x}#aib-role`) : null)}
      headingLevel={3}
      tools={picker}
      footer={
        <p className="text-xs">
          <Link className="link" to={href(pairLink)}>
            Every relationship thesis of {name}, with any partner, on the Partnerships page &rarr;
          </Link>
        </p>
      }
    />
  );
}

/** The shared component with the profile's settings. */
function Lenses({ kind, onKind, lenses, sc, name }: { kind: PartnershipKind; onKind: (k: PartnershipKind) => void; lenses: Record<LensId, LensPanelSpec>; sc: CountryScenario; name?: (iso: string) => string }) {
  const { sheet, partner } = useProfile();
  const href = useProfileHref();
  return (
    <PartnershipLenses
      idPrefix={`ptab-${kind}`}
      kind={kind}
      onKind={onKind}
      kindLabel="Kind of partnership"
      headingLevel={3}
      focus={proseName(sheet.meta.country)}
      noun={{ one: 'partner', many: 'partners' }}
      name={name ?? sc.name}
      partnerHref={href}
      scenarioLabel={sc.label}
      lenses={lenses}
      pair={partner && partner !== sheet.iso3 ? partner : null}
    />
  );
}

// ---- Trade -----------------------------------------------------------------------------------------------

function TradeLenses({ onKind }: { onKind: (k: PartnershipKind) => void }) {
  const { sheet, aib } = useProfile();
  const sc = useCountryScenario();
  const href = useProfileHref();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const rows = sheet.partners.rows;
  const byIso = new Map(rows.map((r) => [r.iso3, r]));
  const doc = sc.doc;
  const nodeName = (id: string) => doc?.nodes.find((nd) => nd.id === id)?.label ?? id;
  const ties = tradeTiesToday(sheet);
  const t = useMemo(() => (sc.compacts ? aibTradeOf(sc.compacts, iso) : null), [sc.compacts, iso]);

  const today: LensPanelSpec = {
    names: rows.length ? lensNames(ties, 10, iso) : lensNone(`${name} has no partner rows`),
    content: rows.length ? (
      <div className="space-y-4">
        <Card id="trade-today" title={`Who ${name} trades with today`}>
          <p>
            Its largest trade partners across its energy-goods baskets in {sheet.meta.trade_year}: the average share of its imports and of its exports (never summed across baskets).
          </p>
          <RankedNames
            rows={ties.slice(0, 8).map((x) => byIso.get(x)).filter((r): r is PartnerRow => !!r)}
            label={`${cap(poss(name))} largest trade partners today`}
            keyOf={(r) => r.iso3}
            nameOf={(r) => r.name}
            href={(r) => href(r.iso3)}
            value={(r) => `rank ${r.ties.tie_rank.d}`}
            note={(r) => `imports ${r.ties.imp_mean.d} · exports ${r.ties.exp_mean.d}`}
          />
        </Card>
        <Fold id="suppliers" summary={`Who supplies ${name} in each sector today`}>
          <SupplierCard />
        </Fold>
      </div>
    ) : null,
    empty: `${cap(name)} has no partner rows.`,
  };
  const osi: LensPanelSpec = {
    names: lensNames(topBy(rows, 'po', 10).map((r) => r.iso3), 10, iso),
    content: (
      <div className="space-y-4">
        <Card id="po-top" title="Export markets (Partnership Opportunity)">
          <p className="text-xs text-paper-mute">{COUNTRY_PARTNER_SCORE_WORDS.po}</p>
          <ScoreList rows={topBy(rows, 'po')} k="po" label="Partnership Opportunity" />
        </Card>
        <Fold id="po-sector" summary="Top export markets sector by sector">
          <PartnersBySector part="po" />
        </Fold>
      </div>
    ),
  };
  const aibSpec: LensPanelSpec = aibPending(sc, aib?.status) ?? {
    names: t ? lensNames(t.pairs.map((p) => p.partner), 10, iso) : LENS_LOADING,
    tools: sc.tools,
    content: (
      <Card id="aib-trade" title="Coordinated trade in the Allied Industrial Base">
        {t && t.pairs.length ? (
          <>
            <p>
              {sc.id === 'managed' ? 'Under Managed Interdependence, i' : 'I'}n the compacts&apos; coordinated trade {name} has {plural(t.trade.n, 'relationship')} worth {fmtUsd(t.trade.usd)}
              {t.trade.nNew ? `, ${t.trade.nNew} of them new` : ''}. The largest:
            </p>
            <ol className="space-y-1.5">
              {t.pairs.slice(0, 5).map((p) => (
                <li key={p.partner}>
                  <PartnerName iso3={p.partner} name={sc.name(p.partner)} /> <span className="tabular text-xs text-paper-mute">{fmtUsd(p.usd)}</span>
                  <span className="block text-xs text-paper-mute">
                    {p.outUsd >= p.inUsd ? `${cap(name)} supplies` : `${cap(name)} buys`} {listWords(p.nodes.slice(0, 2).map(nodeName))}
                    {p.nNew ? ` · ${p.nNew} new` : ''}
                  </span>
                </li>
              ))}
            </ol>
          </>
        ) : (
          <p>
            {cap(name)} has no trade relationship in the compacts&apos; coordinated trade under {sc.label}.
          </p>
        )}
        <ScenarioNote sc={sc} />
      </Card>
    ),
  };
  return <Lenses kind="trade" onKind={onKind} lenses={{ today, osi, aib: aibSpec }} sc={sc} />;
}

// ---- Investment ------------------------------------------------------------------------------------------

function InvestmentLenses({ onKind }: { onKind: (k: PartnershipKind) => void }) {
  const { sheet, aib } = useProfile();
  const sc = useCountryScenario();
  const ties = useTies();
  const iso = sheet.iso3;
  const rows = sheet.partners.rows;
  const today: LensPanelSpec = {
    names: !ties ? LENS_LOADING : ties.ties ? lensNames(dipPartners(ties.ties, iso), 10, iso) : lensNone('the direct-investment positions are not in this build'),
    content: <PartnersBySector part="investment" />,
  };
  const osi: LensPanelSpec = {
    names: lensNames(topBy(rows, 'fs', 10).map((r) => r.iso3), 10, iso),
    content: (
      <div className="space-y-4">
        <Card id="fs-top" title="Friendshoring opportunities (Friendshore)">
          <p className="text-xs text-paper-mute">{COUNTRY_PARTNER_SCORE_WORDS.fs}</p>
          <ScoreList rows={topBy(rows, 'fs')} k="fs" label="Friendshore" />
        </Card>
        <Fold id="fs-sector" summary="Friendshoring opportunities sector by sector">
          <PartnersBySector part="fs" />
        </Fold>
      </div>
    ),
  };
  const pending = aibPending(sc, aib?.status);
  const pairs = useMemo(() => (sc.doc && sc.flows ? investmentPairs({ nodes: sc.doc.nodes, investmentFlows: sc.flows }, { ...NO_FILTER, country: iso }) : []), [sc.doc, sc.flows, iso]);
  const aibSpec: LensPanelSpec = pending ?? {
    names: lensNames(partnersInPairs(pairs.map((p) => ({ producer: p.investor, consumer: p.host })), iso), 10, iso),
    tools: sc.tools,
    content: <AibInvestmentLens sc={sc} pairs={pairs} />,
  };
  return <Lenses kind="investment" onKind={onKind} lenses={{ today, osi, aib: aibSpec }} sc={sc} name={tiesNamer(ties?.ties, sc.name)} />;
}

function Bars({ rows, label, name, nodeName }: { rows: PartnerTotal[]; label: string; name: (iso: string) => string; nodeName: (id: string) => string }) {
  const max = rows.reduce((m, r) => Math.max(m, r.usd), 0);
  return (
    <ol className="space-y-1.5" aria-label={label}>
      {rows.slice(0, 5).map((r) => (
        <li key={r.iso3}>
          <div className="flex items-baseline justify-between gap-3">
            <PartnerName iso3={r.iso3} name={name(r.iso3)} />
            <span className="tabular text-xs text-paper-mute">{fmtUsd(r.usd)}</span>
          </div>
          <span className="block h-1.5 overflow-hidden rounded-sm bg-white/5" aria-hidden="true">
            <span className="block h-full" style={{ width: `${max > 0 ? (r.usd / max) * 100 : 0}%`, background: INVEST_COLOR }} />
          </span>
          <span className="block text-xs text-paper-mute">
            {listWords(r.nodes.slice(0, 2).map(nodeName))}
            {r.nodes.length > 2 ? ` and ${r.nodes.length - 2} more` : ''}
          </span>
        </li>
      ))}
    </ol>
  );
}

/**
 * The investor -> host flows of the toggled scenario with the economy as investor (the hosts whose new
 * capacity serves its lost supply) and as host (the investors its new capacity serves), best-ranked hosts
 * only, then the nodes where it is a ranked host. Without flows, the nodes where it is a listed host.
 */
function AibInvestmentLens({ sc, pairs }: { sc: CountryScenario; pairs: ReturnType<typeof investmentPairs> }) {
  const { sheet } = useProfile();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const doc = sc.doc as AibDoc;
  const asInvestor = useMemo(() => partnerTotals(pairs, iso, 'investor'), [pairs, iso]);
  const asHost = useMemo(() => partnerTotals(pairs, iso, 'host'), [pairs, iso]);
  const ranks = useMemo(() => (sc.flows ? hostRanksOf({ investmentFlows: sc.flows }, iso) : []), [sc.flows, iso]);
  const rows = useMemo(() => lensNodes(doc.nodes, sc.id, sc.flows), [doc, sc.id, sc.flows]);
  const hostNodes = rows.filter((r) => r.hosts.includes(iso));
  const nodeName = (id: string) => doc.nodes.find((nd) => nd.id === id)?.label ?? id;
  const nodeOf = (id: string) => doc.nodes.find((nd) => nd.id === id);
  const clsOf = (id: string) => rows.find((r) => r.node.id === id)?.cls ?? null;
  const maxRank = sc.flows?.variant.maxRank;
  const sum = (xs: PartnerTotal[]) => xs.reduce((s, r) => s + r.usd, 0);
  const best = ranks.filter((h) => h.rank === 1);
  return (
    <section className="card p-5" aria-labelledby="aib-host-h" data-testid="aib-host">
      <h4 id="aib-host-h" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Investment flows in the Allied Industrial Base
      </h4>
      <div className="mt-2 space-y-3 text-sm text-paper-dim">
        {!sc.flows ? (
          hostNodes.length ? (
            <p>
              {cap(name)} is a country that could add capacity in {plural(hostNodes.length, 'supply-chain step')} under {sc.label}: {listWords(hostNodes.slice(0, 4).map((r) => r.node.label))}.
            </p>
          ) : (
            <p>
              {cap(name)} is not listed as a country that could add capacity in any supply-chain step under {sc.label}.
            </p>
          )
        ) : (
          <>
            <p>
              Where a gap remains after coordinated trade, the analysis adds new capacity in the best-placed country that could add capacity for each supply-chain step; the countries
              whose lost supply it replaces are its investors.
            </p>
            <div className="grid gap-4 sm:grid-cols-2">
              <div data-testid="aib-as-investor">
                <div className="text-paper">As investor</div>
                {asInvestor.length ? (
                  <>
                    <p className="mb-1 text-xs text-paper-mute">
                      New capacity at {plural(asInvestor.length, 'host')} serves {fmtUsd(sum(asInvestor))} a year of {poss(name)} lost supply:
                    </p>
                    <Bars rows={asInvestor} label={`Hosts whose new capacity serves ${name}`} name={sc.name} nodeName={nodeName} />
                  </>
                ) : (
                  <p className="text-xs text-paper-mute">No new capacity is simulated for {poss(name)} gaps.</p>
                )}
              </div>
              <div data-testid="aib-as-host">
                <div className="text-paper">As host</div>
                {asHost.length ? (
                  <>
                    <p className="mb-1 text-xs text-paper-mute">
                      New capacity in {name} serves {plural(asHost.length, 'investor')}, {fmtUsd(sum(asHost))} a year:
                    </p>
                    <Bars rows={asHost} label={`Investors served by new capacity in ${name}`} name={sc.name} nodeName={nodeName} />
                  </>
                ) : (
                  <p className="text-xs text-paper-mute">{cap(name)} is not the best-placed country to add capacity in any supply-chain step.</p>
                )}
              </div>
            </div>
            {ranks.length > 0 && (
              <div>
                <div className="text-paper">Where it could add capacity</div>
                <p className="text-xs text-paper-mute">
                  {cap(name)} is ranked as a country that could add capacity in {plural(new Set(ranks.map((h) => h.node)).size, 'supply-chain step')}
                  {best.length ? `, the best placed in ${best.length}` : ''}. Lower ranks are alternatives to the best-placed country, not added supply.
                </p>
                <ul className="mt-1 space-y-1">
                  {ranks.slice(0, 5).map((h) => {
                    const nd = nodeOf(h.node);
                    const cls = clsOf(h.node);
                    return (
                      <li key={`${h.node}|${h.rank}`} className="flex flex-wrap items-baseline gap-x-2 text-xs">
                        <span className="text-paper">{nd?.label ?? h.node}</span>
                        {cls && <ClassChip cls={cls} />}
                        <span className="tabular text-paper-mute">
                          {rankWords(h.rank, maxRank)} · resilience gain {fmtPp(h.gain)}
                        </span>
                        {nd?.compact && (
                          <Link className="link" to={compactHref(nd.compact, sc.id)}>
                            {nd.compact}
                          </Link>
                        )}
                      </li>
                    );
                  })}
                </ul>
              </div>
            )}
            <p className="text-xs">
              <Link className="link" to={withScenario(`/aib/partnerships?mode=investment&country=${iso}`, sc.id)}>
                Every investor-to-host flow of {name} &rarr;
              </Link>
            </p>
          </>
        )}
        <ScenarioNote sc={sc} />
      </div>
    </section>
  );
}

// ---- Development -----------------------------------------------------------------------------------------

function DevelopmentLenses({ onKind }: { onKind: (k: PartnershipKind) => void }) {
  const { sheet, aib } = useProfile();
  const sc = useCountryScenario();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const rows = sheet.partners.rows;
  const [odaNames, setOdaNames] = useState<LensNames>(LENS_LOADING);
  const onOda = useCallback((n: LensNames) => setOdaNames(n), []);
  const href = useProfileHref();
  const doc = sc.doc;
  const lens = useMemo(() => (doc ? lensNodes(doc.nodes, sc.id, sc.flows) : []), [doc, sc.id, sc.flows]);
  const compactIds = useMemo(
    () => (sc.compacts ?? []).filter((c) => (c.members ?? []).includes(iso) || c.roles.some((r) => r.iso3 === iso)).map((c) => c.id),
    [sc.compacts, iso],
  );
  const nodes = useMemo(() => buildNodesFor(lens, compactIds, iso), [lens, compactIds, iso]);
  const buildFlows = useMemo(() => {
    if (!doc || !sc.flows) return [];
    const ids = new Set(nodes.map((n) => n.node.id));
    return investmentPairs({ nodes: doc.nodes, investmentFlows: sc.flows }, { ...NO_FILTER, country: iso }).filter((p) => ids.has(p.node));
  }, [doc, sc.flows, nodes, iso]);
  const today: LensPanelSpec = {
    names: odaNames,
    content: (
      <div className="space-y-3">
        <Suspense fallback={<Loading label="Loading the aid flows" />}>
          <OdaToday iso={iso} focusName={name} stage={null} href={href} onNames={onOda} level={4} />
        </Suspense>
        <LinkRow>
          <Link className="link" to={devDataLink(null)}>
            The development data behind this (World Bank indicators, aid flows, direct investment) &rarr;
          </Link>
        </LinkRow>
      </div>
    ),
  };
  const osi: LensPanelSpec = {
    names: lensNames(topBy(rows, 'dp', 10).map((r) => r.iso3), 10, iso),
    content: (
      <Card id="dp-top" title="Development partners (Development Potential)">
        <p className="text-xs text-paper-mute">{COUNTRY_PARTNER_SCORE_WORDS.dp} Scored for middle- and lower-income partners only.</p>
        <ScoreList rows={topBy(rows, 'dp')} k="dp" label="Development Potential" />
      </Card>
    ),
  };
  const pending = aibPending(sc, aib?.status);
  const partners = [...partnersInPairs(buildFlows.map((p) => ({ producer: p.investor, consumer: p.host })), iso), ...nodes.flatMap((n) => n.hosts)];
  const aibSpec: LensPanelSpec = pending ?? {
    names: lensNames(partners, 10, iso),
    tools: sc.tools,
    content: (
      <Card id="aib-build" title="Capacity to build in its compacts">
        {nodes.length ? (
          <>
            <p>
              Supply-chain steps that need new capacity ({AIB_CLASS_NAME.BUILD}) under {sc.label}, in {poss(name)} compacts or where it could add capacity, largest remaining gap first,
              with the countries that could add it:
            </p>
            <ul className="space-y-1.5">
              {nodes.slice(0, 6).map((nd) => (
                <li key={nd.node.id}>
                  <span className="text-paper">{nd.node.label}</span> <span className="tabular text-xs text-paper-mute">remaining gap {fmtShare(nd.residual, 0)}</span>
                  {nd.node.compact && (
                    <>
                      {' '}
                      <Link className="link text-xs" to={compactHref(nd.node.compact, sc.id)}>
                        {nd.node.compact}
                      </Link>
                    </>
                  )}
                  <span className="block text-xs text-paper-mute">{nd.hosts.length ? `Could add capacity: ${listWords(nd.hosts.slice(0, 4).map(sc.name))}` : 'No country listed to add capacity'}</span>
                </li>
              ))}
            </ul>
          </>
        ) : (
          <p>
            No supply-chain step of {poss(name)} compacts needs new capacity ({AIB_CLASS_NAME.BUILD}) under {sc.label}, and it is not listed to add capacity in one.
          </p>
        )}
        <ScenarioNote sc={sc} />
      </Card>
    ),
  };
  return <Lenses kind="development" onKind={onKind} lenses={{ today, osi, aib: aibSpec }} sc={sc} />;
}

// ---- Geopolitical ----------------------------------------------------------------------------------------

const ROUTE_WORDS: Readonly<Record<string, string>> = { alignment: 'UN-vote alignment', agreement: 'an economic-security agreement', supplier: 'its role as a supplier' };

function GeopoliticalLenses({ onKind }: { onKind: (k: PartnershipKind) => void }) {
  const { sheet, aib, manifest } = useProfile();
  const sc = useCountryScenario();
  const ties = useTies();
  const name = proseName(sheet.meta.country);
  const iso = sheet.iso3;
  const meta = sheet.meta;
  const tiesNow = useMemo(() => (ties?.ties ? agreementTies(ties.ties, iso) : null), [ties, iso]);
  const fromSheet = useMemo(() => agreementRows(sheet), [sheet]);
  const nm = tiesNamer(ties?.ties, sc.name);
  const routes = meta.routes.filter((r) => ROUTE_WORDS[r]).map((r) => ROUTE_WORDS[r]);
  const agreements = [...meta.agreements, ...meta.eu_agreements.map((a) => `${a} (through the EU)`)];
  const today: LensPanelSpec = {
    names: !ties ? LENS_LOADING : tiesNow ? lensNames(tiesNow.map((r) => r.iso3), 40, iso) : lensNames(fromSheet.map((r) => r.iso3), 40, iso),
    content: (
      <Card id="geo-today" title={`${cap(poss(name))} alignment today`}>
        <p>
          {cap(name)} is {tierPhrase(meta.tier)}
          {routes.length ? `, through ${listWords(routes)}` : ''}. In the UN General Assembly it votes {meta.bloc ? `in the ${meta.bloc} group` : 'without a bloc in the data'}
          {meta.alignment_s.v !== null ? ` (position ${meta.alignment_s.d} on a scale from 0, China, to 1, the United States)` : ''}.{' '}
          {agreements.length ? `It is in ${agreementsText(sheet, meta.agreements)}${meta.eu_agreements.length ? ` and, through the EU, ${agreementsText(sheet, meta.eu_agreements)}` : ''}.` : 'It is in no economic-security agreement in the data.'}
        </p>
        {!ties ? (
          <Loading label="Loading the agreements" />
        ) : tiesNow ? (
          tiesNow.length ? (
            <>
              <p className="text-xs text-paper-mute">The economies it shares an agreement with, most agreements first:</p>
              <RankedNames
                rows={tiesNow}
                max={10}
                label={`Economies that share an economic-security agreement with ${name}`}
                keyOf={(r) => r.iso3}
                nameOf={(r) => nm(r.iso3)}
                value={(r) => plural(r.agreements.length, 'agreement')}
                note={(r) => `${agreementNames(r.agreements, sheet.words?.agreement)}; ${alignmentWords(r.alignment)}`}
              />
              {tiesNow.length > 10 && <p className="text-xs text-paper-mute">And {tiesNow.length - 10} more.</p>}
            </>
          ) : (
            <p className="text-paper-mute">{cap(name)} shares no economic-security agreement with another economy in the data.</p>
          )
        ) : fromSheet.length ? (
          <>
            <p className="text-xs text-paper-mute">Among its listed partners, those it shares an agreement with:</p>
            <RankedNames
              rows={fromSheet}
              max={10}
              label={`Partners that share an economic-security agreement with ${name}`}
              keyOf={(r) => r.iso3}
              nameOf={(r) => r.name}
              value={(r) => plural(r.ties.agreements.length, 'agreement')}
              note={(r) => `${agreementsText(sheet, r.ties.agreements)}; ${alignmentText(r)}`}
            />
          </>
        ) : (
          <p className="text-paper-mute">None of its listed partners shares an agreement with it.</p>
        )}
      </Card>
    ),
  };
  const roster = manifest.countries.map((c) => ({ iso3: c.iso3, name: c.name, tier: c.tier }));
  const coalition = coalitionOf(roster);
  const inside = meta.tier === 'core' || meta.tier === 'partner';
  const core = coalition.filter((c) => c.tier === 'core');
  const partner = coalition.filter((c) => c.tier === 'partner');
  const fsTop = topBy(sheet.partners.rows, 'fs', 8);
  const osi: LensPanelSpec = {
    names: inside ? lensNames(coalition.map((c) => c.iso3), 200, iso) : lensNone(`${name} is outside the OSI's allied coalition`),
    content: (
      <div className="space-y-4">
        <Card id="geo-osi" title="The allied coalition">
          <p>
            The indices treat {plural(core.length, 'core member')} and {plural(partner.length, 'partner')} as the allied coalition, reached through UN-vote alignment, an economic-security
            agreement or a supplier route. {cap(name)} is {tierWords(meta.tier)}.
          </p>
          <p className="text-xs text-paper-mute">
            <span className="text-paper-dim">Core members:</span> {core.map((c) => c.name).join(', ') || 'none'}.
          </p>
          <p className="text-xs text-paper-mute">
            <span className="text-paper-dim">Partners:</span> {partner.map((c) => c.name).join(', ') || 'none'}.
          </p>
        </Card>
        <Card id="geo-osi-fs" title="Friendshore partners and how they vote">
          <p className="text-xs text-paper-mute">Friendshore weighs UN-vote alignment as one of its parts; {poss(name)} strongest Friendshore partners, with their coalition membership and alignment.</p>
          {fsTop.length ? (
            <RankedNames
              rows={fsTop}
              label={`${cap(poss(name))} Friendshore partners`}
              keyOf={(r) => r.iso3}
              nameOf={(r) => r.name}
              value={(r) => r.fs.d}
              note={(r) => `${tierWords(r.tier)}; ${alignmentText(r)}`}
            />
          ) : (
            <p className="text-paper-mute">No partner has a Friendshore score for {name}.</p>
          )}
        </Card>
      </div>
    ),
  };
  const pending = aibPending(sc, aib?.status);
  const co = useMemo(() => (sc.compacts && sc.doc ? lensCoalitions(sc.compacts, sc.doc.nodes, { ...NO_FILTER, country: iso }) : null), [sc.compacts, sc.doc, iso]);
  const aibSpec: LensPanelSpec = pending ?? {
    names: co ? lensNames(co.members.map((m) => m.iso3), 60, iso) : LENS_LOADING,
    tools: sc.tools,
    content: co ? <AibCoalitionCard sc={sc} co={co} /> : <Loading label="Loading the compact coalitions" />,
  };
  return <Lenses kind="geopolitical" onKind={onKind} lenses={{ today, osi, aib: aibSpec }} sc={sc} name={nm} />;
}

function AibCoalitionCard({ sc, co }: { sc: CountryScenario; co: ReturnType<typeof lensCoalitions> }) {
  const { sheet } = useProfile();
  const href = useProfileHref();
  const name = proseName(sheet.meta.country);
  const doc = sc.doc as AibDoc;
  const titleOf = (id: string) => {
    const c = doc.compacts.find((x) => x.id === id);
    return c ? compactTitle(c, doc.nodes) : id;
  };
  return (
    <Card id="aib-coalitions" title="Compact coalitions">
      <p>
        {co.seats.length
          ? `${cap(name)} sits in the coalition of ${plural(co.seats.length, 'compact')} under ${sc.label}${co.dependency.length ? `, and ${co.dependency.length === 1 ? 'one more compact depends' : `${co.dependency.length} more compacts depend`} on it from outside` : ''}.`
          : co.dependency.length
            ? `${cap(name)} sits in no compact coalition under ${sc.label}, but ${plural(co.dependency.length, 'compact')} ${co.dependency.length === 1 ? 'depends' : 'depend'} on it from outside as a critical dependency.`
            : `${cap(name)} sits in no compact coalition under ${sc.label}.`}
      </p>
      {co.seats.length > 0 && (
        <ul className="space-y-1 text-xs text-paper-mute" aria-label={`Compacts whose coalition ${name} sits in`}>
          {co.seats.map((id) => (
            <li key={id}>
              <Link className="link" to={compactHref(id, sc.id)}>
                {id}
              </Link>{' '}
              {titleOf(id)}
            </li>
          ))}
        </ul>
      )}
      {co.members.length > 0 && (
        <>
          <p className="text-xs text-paper-mute">Its co-members, most compacts first:</p>
          <RankedNames
            rows={co.members}
            max={10}
            label={`${cap(poss(name))} co-members in the compact coalitions`}
            keyOf={(m) => m.iso3}
            nameOf={(m) => sc.name(m.iso3)}
            href={(m) => href(m.iso3)}
            value={(m) => plural(m.n, 'compact')}
          />
        </>
      )}
      {co.dependency.length > 0 && (
        <p className="text-xs text-paper-mute">
          A critical dependency outside the coalition of {listWords(co.dependency)}: losing its supply would push a supply-chain step below the target.
        </p>
      )}
      {co.criticalExternal.length > 0 && co.seats.length > 0 && (
        <p className="text-xs text-paper-mute">Critical dependencies outside its coalitions: {listWords(co.criticalExternal.slice(0, 6).map(sc.name))}.</p>
      )}
      <ScenarioNote sc={sc} />
    </Card>
  );
}

// ---- The partner-by-partner disclosures (unchanged) -------------------------------------------------------

function SlopeCard() {
  const { sheet, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const m = useMemo(() => slopeModel(sheet), [sheet]);
  const union = [...new Set([...m.left.map((s) => s.iso3), ...m.right.map((s) => s.iso3)])];
  const rows = new Map(sheet.partners.rows.map((r) => [r.iso3, r]));
  type U = { iso3: string; name: string; group: string; psi_rank: string; psi: string; trade_rank: string; imports: string; exports: string };
  const twin: U[] = union.map((iso) => {
    const r = rows.get(iso)!;
    return { iso3: iso, name: r.name, group: GROUP_WORD[r.group], psi_rank: r.rankable ? r.rank.d : 'not ranked', psi: r.psi.d, trade_rank: r.ties.tie_rank.d, imports: r.ties.imp_mean.d, exports: r.ties.exp_mean.d };
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Priority partners and today's partners"
      sub={`Left: the partners the index rates highest for ${name}. Right: the partners it trades most with today across its energy-goods baskets (average share of its imports and of its exports in ${sheet.meta.trade_year}, never summed across baskets). Lines join the same partner.`}
      summary={`${cap(name)}: the top ${m.left.length} partners by Partnership Strength and the top ${m.right.length} by average trade share.`}
      table={() => (
        <TableView<U>
          rows={twin}
          cols={[
            { id: 'name', header: 'Partner', value: (r) => r.name },
            { id: 'group', header: 'Group', value: (r) => r.group },
            { id: 'psi_rank', header: 'Partnership Strength rank', value: (r) => r.psi_rank },
            { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi, num: true },
            { id: 'trade_rank', header: 'Trade rank', value: (r) => r.trade_rank },
            { id: 'imports', header: 'Share of imports (average)', value: (r) => r.imports, num: true },
            { id: 'exports', header: 'Share of exports (average)', value: (r) => r.exports, num: true },
          ]}
          caption={`${cap(name)}: priority partners and trade partners`}
          rowKey={(r) => r.iso3}
          onRowClick={(r) => setPartner(r.iso3)}
          selectedKey={partner}
        />
      )}
    >
      <PartnerSlope left={m.left} right={m.right} selected={partner} onSelect={setPartner} />
    </ChartCard>
  );
}

function PartnerTable() {
  const { sheet, cell, partner, setPartner } = useProfile();
  const pc = partner ? cellOf(sheet, cell) : null;
  const name = proseName(sheet.meta.country);
  const rows = useMemo(() => partnerTableRows(sheet), [sheet]);
  const cols: Col<Row>[] = [
    { id: 'partner', header: 'Partner', value: (r) => r.name, cell: (r) => <span>{r.name} <span className="text-xs text-paper-mute">({GROUP_WORD[r.group]}){r.extra ? ' · trade partner' : ''}</span></span>, minWidth: 150 },
    { id: 'rank', header: 'Rank', value: (r) => (r.rankable ? r.rank.v : null), cell: (r) => (r.rankable ? r.rank.d : `not ranked${r.rank_reason ? `: ${r.rank_reason}` : ''}`), num: true },
    { id: 'psi', header: 'Partnership Strength', value: (r) => r.psi.v, cell: (r) => r.psi.d, num: true },
    { id: 'reason', header: 'Main reason', value: (r) => partnerReason(sheet, r), minWidth: 200 },
    { id: 'fs', header: 'Friendshore', value: (r) => r.fs.v, cell: (r) => r.fs.d, num: true },
    { id: 'po', header: 'Partnership Opportunity', value: (r) => r.po.v, cell: (r) => r.po.d, num: true },
    { id: 'dp', header: 'Development Potential', value: (r) => r.dp.v, cell: (r) => r.dp.d, num: true, minWidth: 120 },
    { id: 'imp', header: 'Imports from', value: (r) => r.ties.imp_mean.v, cell: (r) => `${r.ties.imp_mean.d}${(r.ties.n_import_cells.v ?? 0) > 0 ? `; top-3 supplier in ${r.ties.top3_import_cells.d} of ${r.ties.n_import_cells.d}` : ''}`, num: true, minWidth: 150 },
    { id: 'exp', header: 'Exports to', value: (r) => r.ties.exp_mean.v, cell: (r) => r.ties.exp_mean.d, num: true },
    { id: 'fdi', header: 'Factory projects', value: (r) => (r.ties.fdi_in.v ?? 0) + (r.ties.fdi_out.v ?? 0), cell: (r) => `${r.ties.fdi_in.d} in ${name} · ${r.ties.fdi_out.d} by ${poss(name)} firms there`, minWidth: 170 },
    { id: 'agreements', header: 'Agreements', value: (r) => agreementsText(sheet, r.ties.agreements), minWidth: 140 },
    { id: 'un', header: 'UN votes', value: (r) => alignmentText(r), cell: (r) => <span title={r.ties.bloc ? `${r.name}: ${r.ties.bloc}` : undefined}>{alignmentText(r)}</span> },
    { id: 'match', header: 'Match', value: (r) => matchText(sheet, r), minWidth: 160 },
  ];
  const csv = () => ({
    name: `${sheet.meta.country} partners`,
    text: csvWithSource(
      rows.map((r) => ({
        partner: r.name, iso3: r.iso3, group: GROUP_WORD[r.group], rank: r.rankable ? r.rank.v : null, partnership_strength: r.psi.v, main_reason: partnerReason(sheet, r),
        friendshore: r.fs.v, partnership_opportunity: r.po.v, development_potential: r.dp.v, imports_share_average: r.ties.imp_mean.v, exports_share_average: r.ties.exp_mean.v,
        top3_supplier_in: r.ties.top3_import_cells.v, import_baskets: r.ties.n_import_cells.v, trade_rank: r.ties.tie_rank.v, factory_projects_in: r.ties.fdi_in.v,
        factory_projects_out: r.ties.fdi_out.v, agreements: agreementsText(sheet, r.ties.agreements), un_votes: alignmentText(r), match: r.match,
      })),
      'Opportunity Security Indices, NEIS baseline; UN Comtrade (reconciled); CM-FDI; agreements; UN General Assembly votes',
      sheet.meta.as_of,
    ),
  });
  return (
    <ChartCard
      headingLevel={3}
      title="Partner by partner"
      sub={`The top 15 partners by Partnership Strength from ${poss(name)} viewpoint, plus any of its ten largest trade partners not among them (marked "trade partner"). Development Potential is scored only for middle- and lower-income partners. Factory projects are cross-border clean-tech manufacturing projects (solar, wind, batteries, electric vehicles). Click a row to select the partner.`}
      summary={`${cap(name)}: ${rows.length} partners with their Partnership Strength, components and today's ties.`}
      csv={csv}
      notes={
        <span className="text-sm" data-testid="dev-data-link">
          <Link className="link" to={devDataLink(partner ?? null, pc ? { tech: pc.tech, stage: pc.stage } : null)}>
            The development data behind this &rarr;
          </Link>
          <span className="text-paper-mute">
            {pc && partner
              ? ` the selected partner's aid received and given, and its direct-investment positions, in Sector Insights (${pc.label})`
              : ' World Bank development indicators, OECD aid flows and IMF direct-investment positions behind Development Potential, in Sector Insights'}
          </span>
        </span>
      }
    >
      <TableView<Row> rows={rows} cols={cols} caption={`${cap(name)}: partners by Partnership Strength`} rowKey={(r) => r.iso3} onRowClick={(r) => setPartner(r.iso3)} selectedKey={partner} search pageSize={15} initialSort={{ id: 'rank' }} />
    </ChartCard>
  );
}

function SupplierCard() {
  const { sheet, partner, setPartner } = useProfile();
  const name = proseName(sheet.meta.country);
  const m = useMemo(() => supplierModel(sheet), [sheet]);
  type L = { partner: string; sector: string; share: number | null; value: number | null; status: string | null };
  const long: L[] = [];
  m.partners.forEach((p, i) =>
    m.cells.forEach((c, j) => {
      const s = m.share[i]?.[j] ?? null;
      if (s !== null) long.push({ partner: p.name, sector: c.label, share: s, value: m.value[i]?.[j] ?? null, status: m.status[i]?.[j] ?? null });
    }),
  );
  const pct = (x: number | null) => (x === null ? '–' : `${(100 * x).toFixed(1)}%`);
  return (
    <ChartCard
      headingLevel={3}
      title={`Who supplies ${name} in each sector (${sheet.meta.trade_year})`}
      sub={`Each dot is one supplier's share of ${poss(name)} imports in a sector (bigger dot, bigger share). Rows: the largest suppliers of its high-priority sectors and its largest suppliers on average. The selected partner's row is outlined; a star marks the sectors among its Friendshore best five for ${name}.`}
      summary={`${cap(name)}: import shares by supplier and sector, ${sheet.meta.trade_year}.`}
      table={() => (
        <TableView<L>
          rows={long}
          cols={[
            { id: 'partner', header: 'Supplier', value: (r) => r.partner },
            { id: 'sector', header: 'Sector', value: (r) => r.sector },
            { id: 'share', header: 'Share of imports', value: (r) => r.share, cell: (r) => pct(r.share), num: true },
            { id: 'value', header: 'Value (USD)', value: (r) => r.value, cell: (r) => (r.value === null ? '–' : Math.round(r.value).toLocaleString('en-US')), num: true },
            { id: 'status', header: 'Recorded by', value: (r) => (r.status === 'mirror' ? 'the supplier (mirror)' : 'the importer') },
          ]}
          caption={`${cap(name)}: import shares by supplier and sector`}
          search
        />
      )}
      csv={() => ({
        name: `${sheet.meta.country} suppliers by sector`,
        text: csvWithSource(long.map((r) => ({ supplier: r.partner, sector: r.sector, import_share: r.share, value_usd: r.value, recorded_by: r.status })), 'UN Comtrade, reconciled bilateral matrix', sheet.meta.as_of),
      })}
      notes="Where a flow was not reported by the importer it is read from the exporter's report, raised by 6% to value it like imports."
    >
      <SupplierMatrix sheet={sheet} selected={partner} onSelect={setPartner} />
    </ChartCard>
  );
}
