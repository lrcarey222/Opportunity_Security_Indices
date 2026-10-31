// Shared pieces of the Strategic Sectoral Compact pages (2026-10-20): the landing (/compacts) and one
// compact (/compacts/s/:sid). Plain words for readers who do not work with the model: a compact's
// persistent parts (its supply chain and core countries) are kept apart from what the toggled scenario
// changes (its mission, policy mix, active workstreams and result). Every reading is the export's
// (doc.strategic through derive/strategic.ts); nothing is scored or re-derived here.

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { COMPACT, SCENARIO_NAME } from '../../../content/vocabulary.ts';
import { SpineStatusSwatch } from '../../charts/ValueChainSpine.tsx';
import type { AibStrategic, AibStrategicCompact, AibStrategicCompactScenario, AibStrategicCoreEntry } from '../../contract.ts';
import { listWords, plural } from '../../derive/format.ts';
import { compactNodeIds, spineStatusInfo, statusCounts, strategicScenarioDef } from '../../derive/strategic.ts';

/**
 * A data table's row header in body text (.data-table th is unlayered, upper case, nowrap and sticky, so
 * utilities cannot override it: an inline style, as AibRolesView and OverviewSections do).
 */
export const ROW_TH_STYLE = { position: 'static', textTransform: 'none', letterSpacing: 'normal', fontSize: '0.8125rem', fontWeight: 400, whiteSpace: 'normal', background: 'transparent' } as const;

/** The module eyebrow of the strategic pages (AibOverviewView's AIB_EYEBROW, repeated so the landing does not load the Overview). */
export const STRATEGIC_EYEBROW = 'ALLIED INDUSTRIAL BASE';

/** An economy's role in the Allied Industrial Base on its Country profile. */
export const countryRoleHref = (iso3: string): string => `/countries/${iso3.toUpperCase()}#aib-role`;

/** The first letter upper case ("build missing ..." -> "Build missing ..."). */
export const upperFirst = (s: string): string => (s ? s.charAt(0).toUpperCase() + s.slice(1) : s);

/**
 * The first letter lower case when the first word is an ordinary capitalized word of three letters or more
 * ("Demand anchor" -> "demand anchor", "Rare-earth ore" -> "rare-earth ore"); an acronym, a chemical symbol
 * or a mixed-case word keeps its case ("AC generators", "PGMs", "Li-ion cells", "NdPr metal"). Not for
 * country names: the callers pass roles, steps and node labels.
 */
export const lowerFirst = (s: string): string => (/^[A-Z][a-z]{2,}(?:[-\s,&/]|$)/.test(s) ? s.charAt(0).toLowerCase() + s.slice(1) : s);

/**
 * The seven sections of a Strategic Sectoral Compact page as questions (the guide, rule 15: why this
 * bottleneck, why these partners, what changes if they coordinate, what action follows).
 */
export const STRATEGIC_SECTION_TITLE = {
  why: COMPACT.sectionTitles.why,
  chain: COMPACT.sectionTitles.what,
  vulnerabilities: 'Where is it vulnerable?',
  policy: COMPACT.sectionTitles.next,
  who: 'Why these partners, and who does what?',
  workstreams: 'Where does the work happen?',
  scenarios: 'What changes under each scenario?',
} as const;

/** The display name of the shared scenarios (content/vocabulary.ts SCENARIO_NAME). */
const DISPLAY_NAME: Readonly<Record<string, string>> = SCENARIO_NAME;

/**
 * The scenario's display name ("Today's network", "Managed Interdependence", "Major-supplier disruption";
 * a scenario only the strategic layer has, such as the partial disruption, keeps the data's label; the id
 * when the section does not list it).
 */
export const scenarioLabelOf = (s: AibStrategic, id: string): string => DISPLAY_NAME[id] ?? strategicScenarioDef(s, id)?.label ?? id;

/** The scenario's label as the data write it ("Baseline", "Decoupling"): the prefix of its headlines. */
export const scenarioDataLabelOf = (s: AibStrategic, id: string): string => strategicScenarioDef(s, id)?.label ?? id;

/**
 * The headline without its scenario prefix ("Managed Interdependence: 3% of ..." -> "3% of ..."; the data
 * write the method's name, "Decoupling: ..."), for a place that already names the scenario. The data's
 * "No shock:" (Today's network) reads "No disruption:".
 */
export function headlineBody(headline: string | null | undefined, ...labels: string[]): string {
  if (!headline) return '–';
  const p = labels.map((l) => `${l}: `).find((x) => headline.startsWith(x));
  const body = p ? upperFirst(headline.slice(p.length)) : headline;
  return body.replace(/^No shock: /, 'No disruption: ');
}

/** How many of the compact's nodes need an intervention (BUILD, COORDINATE or MANAGE SPECIALIZATION) under a scenario. */
export const interventionCount = (sc: AibStrategicCompactScenario | null): number => (sc ? sc.counts.build + sc.counts.coordinate + sc.counts.manage : 0);

/** "1 of 9 supply-chain steps needs action" (none: "no step of 9 needs action"; Today's network: "no disruption applied"). */
export function interventionWords(c: AibStrategicCompact, sc: AibStrategicCompactScenario | null): string {
  if (!sc) return '–';
  const total = compactNodeIds(c).length;
  if (sc.counts.noShock > 0 && sc.counts.noShock === total) return 'no disruption applied';
  const n = interventionCount(sc);
  if (n === 0) return `no step of ${total} needs action`;
  return `${n} of ${plural(total, 'supply-chain step')} ${n === 1 ? 'needs' : 'need'} action`;
}

/** The active-workstream line ("1 of 5 workstreams active"; "No workstream: ..."). */
export function activeWords(c: AibStrategicCompact, active: readonly string[]): string {
  const n = c.workstreams.length;
  if (n === 0) return 'No workstream: none of the analysis’s clusters falls mainly in this compact';
  return `${active.length} of ${plural(n, 'workstream')} active`;
}

/** The compact's nodes by status under a scenario in words ("1 BUILD, 3 LOW INTERVENTION, 4 NOT STRESSED"; the words of a mini spine). */
export const countsWords = (sc: AibStrategicCompactScenario | null): string =>
  statusCounts(sc)
    .map((r) => `${r.n} ${spineStatusInfo(r.status).label}`)
    .join(', ');

/**
 * The compact's nodes as small squares in value-chain order (a gap between steps), each coloured by its
 * status under the scenario; drawn only (aria-hidden): the caller prints the counts.
 */
export function MiniSpine({ c, sc }: { c: AibStrategicCompact; sc: AibStrategicCompactScenario | null }) {
  const status = new Map((sc?.nodes ?? []).map((t) => [t[0], t[1]]));
  return (
    <span aria-hidden="true" className="inline-flex flex-wrap items-center gap-x-2 gap-y-1" data-testid="mini-spine">
      {c.steps.map((st) => (
        <span key={st.order} className="inline-flex gap-0.5">
          {st.nodes.map((id) => (
            <SpineStatusSwatch key={id} status={status.get(id) ?? null} size={8} />
          ))}
        </span>
      ))}
    </span>
  );
}

/** The supply-chain span as an ordered list of its steps (wraps on a phone). */
export function SpanList({ c, className = '' }: { c: AibStrategicCompact; className?: string }) {
  return (
    <ol className={`flex flex-wrap items-center gap-x-1.5 gap-y-0.5 ${className}`} aria-label={`Supply chain of ${c.name}, in order`}>
      {c.steps.map((st, i) => (
        <li key={st.order} className="inline-flex items-center gap-1.5" title={st.description ?? undefined}>
          {i > 0 && (
            <span aria-hidden="true" className="text-paper-mute">
              →
            </span>
          )}
          <span>{st.label}</span>
        </li>
      ))}
    </ol>
  );
}

/** "Japan — demand anchor" for each core country; `link`: the names link to their Allied Industrial Base role. */
export function CoreCountries({
  core, name, href, withStep = false, label,
}: {
  core: readonly AibStrategicCoreEntry[];
  name: (iso3: string) => string;
  /** an href with the toggle carried (names link to the Country profile's role) or null for plain names */
  href: ((to: string) => string) | null;
  withStep?: boolean;
  label: string;
}) {
  if (!core.length) return <span className="text-paper-mute">none identified</span>;
  return (
    <ul className="flex flex-col gap-0.5" aria-label={label}>
      {core.map((x) => (
        <li key={x.iso3}>
          {href ? (
            <Link className="link" to={href(countryRoleHref(x.iso3))}>
              {name(x.iso3)}
            </Link>
          ) : (
            <span className="text-paper">{name(x.iso3)}</span>
          )}{' '}
          <span className="text-paper-mute">—</span> {lowerFirst(x.role)}
          {withStep && x.step ? <span className="text-paper-mute"> ({lowerFirst(x.step)})</span> : null}
        </li>
      ))}
    </ul>
  );
}

/** Economy names as words ("Korea, Viet Nam and Japan"); `none` when the list is empty, `na` when it is null (not assessed). */
export function namesWords(list: readonly string[] | null | undefined, name: (iso3: string) => string, none = 'none', na = 'not assessed under this scenario'): string {
  if (list === null || list === undefined) return na;
  return list.length ? listWords(list.map(name)) : none;
}

/** A small labelled block of a card or panel (a term and its description). */
export function Fact({ term, children }: { term: string; children: ReactNode }) {
  return (
    <>
      <dt className="text-paper-mute">{term}</dt>
      <dd className="min-w-0 text-paper-dim">{children}</dd>
    </>
  );
}
