// Helpers shared by the views: row caches, the tech / stage filters, the Show-mode wording, the
// economy options and small derivations.

import { useEffect, useMemo } from 'react';
import { Link } from 'react-router';
import { toRows } from '../engine/table.ts';
import type { Table } from '../engine/types.ts';
import { useApp, useScenario } from '../state/EngineProvider.tsx';
import type { AppData } from '../state/appData.ts';
import type { ShowMode } from '../state/url.ts';
import { Select } from '../components/ui.tsx';
import type { CountryOption } from '../components/CountrySelect.tsx';
import { pageTitle } from '../shell/title.ts';

/** The view's document title, unless it is embedded in another page that owns the title (IA refactor 2026-10-16). */
export function useViewTitle(view: string, embedded?: boolean): void {
  useEffect(() => {
    if (!embedded) document.title = pageTitle(view);
  }, [view, embedded]);
}

export type Row = Record<string, string | number | boolean | null>;

const rowCache = new WeakMap<Table, Row[]>();

/** Row objects of a columnar table, computed once per table object. */
export function rowsOf(t: Table | null | undefined): Row[] {
  if (!t) return [];
  let r = rowCache.get(t);
  if (!r) {
    r = toRows(t);
    rowCache.set(t, r);
  }
  return r;
}

export const numOr = (v: unknown): number | null => (typeof v === 'number' && Number.isFinite(v) ? v : null);
export const strOr = (v: unknown): string | null => (typeof v === 'string' ? v : null);

/** The view's technology and stage (URL, else the defaults). */
export function useTechStage(): { tech: string; stage: string; setTech: (t: string) => void; setStage: (s: string) => void } {
  const app = useApp();
  const { view, setView } = useScenario();
  return {
    tech: view.tech ?? app.defaultTech,
    stage: view.stage ?? app.defaultStage,
    setTech: (t) => setView({ tech: t === app.defaultTech ? null : t }),
    setStage: (s) => setView({ stage: s === app.defaultStage ? null : s }),
  };
}

export function TechStageFilters({ techLabel = 'Technology', stageDisabled, techDisabled, title }: { techLabel?: string; stageDisabled?: boolean; techDisabled?: boolean; title?: string }) {
  const app = useApp();
  const { tech, stage, setTech, setStage } = useTechStage();
  return (
    <>
      <Select label={techLabel} value={tech} options={app.techs.map((t) => ({ value: t, label: t }))} onChange={setTech} disabled={techDisabled} title={title} />
      <Select label="Stage" value={stage} options={app.stages.map((s) => ({ value: s, label: s }))} onChange={setStage} disabled={stageDisabled} className="min-w-[9rem]" title={title} />
    </>
  );
}

export const showWord = (m: ShowMode): string => (m === 'baseline' ? 'Paper baseline values.' : m === 'change' ? 'Scenario minus the paper baseline.' : 'Scenario values.');

/** Economy options grouped by coalition tier (for CountrySelect). */
export function useCountryOptions(only?: (c: AppData['countries'][number]) => boolean): CountryOption[] {
  const app = useApp();
  return useMemo(() => {
    const group = (t: string) => (t === 'core' ? 'Coalition core' : t === 'partner' ? 'Coalition partners' : 'Other economies');
    const order = (t: string) => (t === 'core' ? 0 : t === 'partner' ? 1 : 2);
    return app.countries
      .filter((c) => (only ? only(c) : true))
      .sort((a, b) => order(a.tier) - order(b.tier) || a.country.localeCompare(b.country, 'en'))
      .map((c) => ({ iso3c: c.iso3c, country: c.country, group: group(c.tier) }));
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [app]);
}

/** Pick the shown value for a Show mode. */
export function shownValue(value: number | null, base: number | null, mode: ShowMode): number | null {
  if (mode === 'baseline') return base;
  if (mode === 'change') return value !== null && base !== null ? value - base : null;
  return value;
}

/** The core payload's notes in reader-facing words. */
export function noteText(n: string): string {
  if (/fossil/i.test(n)) return 'Coal, oil and gas are excluded from the Strategic Index and the partnership best-five summaries.';
  if (/endowment screen off/i.test(n)) return 'The endowment screen is off: an upstream sector can be a member’s allied pick without recorded reserves or production of its resources.';
  if (/baseline pillars/i.test(n)) return "Partnerships keep the paper's Energy Security and Economic Opportunity (category weights move those two scores and the Strategic Index only).";
  return n;
}

/** What an economy's Policy activity rests on (policy_basis), in reader-facing words. */
export function policyBasisText(basis: string | null | undefined): string {
  if (!basis) return 'no Policy activity score';
  if (basis === 'IEA PAMS only') return 'IEA Policies and Measures only (no Climate Action Tracker rating)';
  if (basis === 'IEA PAMS + CAT') return 'IEA Policies and Measures and the Climate Action Tracker';
  return basis.replace(/\bIEA PAMS\b/, 'IEA Policies and Measures').replace(/\bCAT\b/, 'Climate Action Tracker');
}

/** A short name of a policy basis for tables and tiles (no bare acronyms). */
export function policyBasisShort(basis: string | null | undefined): string | null {
  if (!basis) return null;
  if (basis === 'IEA PAMS only') return 'IEA records only';
  if (basis === 'IEA PAMS + CAT') return 'IEA records + Climate Action Tracker';
  return basis.replace(/\bIEA PAMS\b/, 'IEA records').replace(/\bCAT\b/, 'Climate Action Tracker');
}

/** Reader-facing names of the Policy activity components (config keys of the policy weights). */
export function policyComponentName(key: string): string {
  if (/iea/i.test(key)) return 'IEA Policies and Measures records';
  if (/(^|[^a-z])cat([^a-z]|$)|climate action/i.test(key)) return 'Climate Action Tracker rating';
  return key;
}

/**
 * The bridge from a Strategic view to the same economy in Country Insights (spec_country.md A2.2): one
 * quiet link, "{Country} in Country Insights ->", to /countries/ISO3 (the paper's baseline there; the
 * Strategic scenario is not carried, Country Insights always shows the paper).
 */
export function CountryInsightsLink({ iso3, country }: { iso3: string; country: string }) {
  return (
    <p className="mt-3 text-sm">
      <Link className="link" to={`/countries/${iso3}`}>
        {country}&rsquo;s Country profile &rarr;
      </Link>
      <span className="text-paper-mute"> (a plain-language briefing on the NEIS baseline)</span>
    </p>
  );
}

/** The endowment screen of a cell (resource_base): true / false in a screened upstream sector, null elsewhere. */
export function resourceBaseText(rb: unknown, evidence?: unknown): string {
  if (rb === true) return typeof evidence === 'string' && evidence ? `yes (${evidence.toLowerCase()})` : 'yes';
  if (rb === false) return 'none recorded';
  return 'not screened';
}
