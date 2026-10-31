// The shared three-lens partnerships component (2026-10-19; aib_scenarios_spec.md s.5 C and s.6), used by
// the Allied Industrial Base Partnerships page and the Country profile's Partnerships section: the kind of
// partnership (Trade | Investment | Development | Geopolitical, a radio group), the cross-lens summary of
// that kind (which partners each lens names and where the lenses agree or differ, in plain words), then
// three clearly tagged panels, Today, OSI ranking and AIB model (the toggled scenario). The pages give
// each panel its content and the partners it names; a lens with no data at the focus shows one line
// instead (never a blank panel) and is left out of the comparison. HTML only: the panels stack at any width,
// names are links where the page gives one, and every reading is text (no colour carries meaning). With a
// pair (2026-10-20: `pair`, the partner of the relationship thesis above), the summary adds one line on
// where that partner sits in the compared lenses.

import { useId, type ReactNode } from 'react';
import { Link } from 'react-router';
import { Segmented } from '../components/ui.tsx';
import { BASIS_WORDS, type Basis } from '../aib/views/basis.tsx';
import {
  comparedWords, crossLens, KIND_LABEL, KIND_LENS_WORDS, LENS_IDS, LENS_INFO, PARTNERSHIP_KINDS, pairReading, summaryLine, type LensId, type LensNames, type PartnershipKind,
} from './lenses.ts';

/** The basis tag of each lens (the AIB section's tag words: views/basis.tsx). */
const LENS_BASIS: Readonly<Record<LensId, Basis>> = { today: 'today', osi: 'baseline', aib: 'model' };

/** How many names a reading lists before "and N more". */
export const SUMMARY_NAMES = 6;

export interface LensPanelSpec {
  /** the panel's body (null or absent: the `empty` line instead) */
  content?: ReactNode;
  /** one line when the lens has no data at this focus (shown when there is no content) */
  empty?: string | null;
  /** the partners the lens names here (the cross-lens summary) */
  names: LensNames;
  /** controls or a note under the panel heading (the scenario toggle on the Country profile) */
  tools?: ReactNode;
}

export interface PartnershipLensesProps {
  /** a prefix for element ids (unique on the page) */
  idPrefix: string;
  kind: PartnershipKind;
  onKind: (k: PartnershipKind) => void;
  /** the kind control's label */
  kindLabel?: string;
  /** the level of the summary and panel headings (the cards inside use the next one where they can) */
  headingLevel: 2 | 3;
  /** the focus in words ("Japan", "all economies and sub-sectors") */
  focus: string;
  /** what the summary counts ("partner" / "partners", "economy" / "economies") */
  noun: { one: string; many: string };
  /** an economy's display name */
  name: (iso3: string) => string;
  /** the link of an economy's name in the summary (null: plain text) */
  partnerHref?: (iso3: string) => string | null;
  /** the AIB model's scenario ("Decoupling"): its tag and heading name it */
  scenarioLabel: string | null;
  lenses: Readonly<Record<LensId, LensPanelSpec>>;
  /** shown after the three panels (folds, links) */
  footer?: ReactNode;
  /** the pair's other economy (2026-10-20): the summary says where it sits in the three lenses */
  pair?: string | null;
}

const KIND_OPTIONS = PARTNERSHIP_KINDS.map((k) => ({ value: k, label: KIND_LABEL[k] }));

/** A lens's tag (the AIB section's basis words, the scenario after the model's). */
export function LensTag({ lens, scenario }: { lens: LensId; scenario?: string | null }) {
  const b = LENS_BASIS[lens];
  const w = BASIS_WORDS[b];
  return (
    <span className="eyebrow block" title={w.title} data-basis={b} data-lens-tag={lens}>
      {w.label}
      {lens === 'aib' && scenario ? ` · ${scenario}` : ''}
    </span>
  );
}

function Names({ keys, name, href }: { keys: readonly string[]; name: (iso3: string) => string; href?: (iso3: string) => string | null }) {
  const shown = keys.slice(0, SUMMARY_NAMES);
  const more = keys.length - shown.length;
  return (
    <>
      {shown.map((k, i) => {
        const to = href?.(k) ?? null;
        const sep = i === 0 ? '' : i === shown.length - 1 && !more ? ' and ' : ', ';
        return (
          <span key={k}>
            {sep}
            {to ? (
              <Link className="link" to={to}>
                {name(k)}
              </Link>
            ) : (
              <span className="text-paper">{name(k)}</span>
            )}
          </span>
        );
      })}
      {more > 0 && <span> and {more} more</span>}
    </>
  );
}

/** One line: where the pair's other economy sits in the compared lenses (nothing before a lens is compared). */
function PairLine({ cl, pair, name, noun }: { cl: ReturnType<typeof crossLens>; pair: string; name: (iso3: string) => string; noun: { one: string; many: string } }) {
  const reading = pairReading(cl, pair);
  if (reading === undefined) return null;
  return (
    <p data-testid="cross-lens-pair">
      <span className="font-medium text-paper">Where {name(pair)} sits:</span>{' '}
      {reading
        ? `${reading}.`
        : `not among the ${noun.many} ${cl.compared.length === 1 ? LENS_INFO[cl.compared[0]].title : comparedWords(cl.compared)} ${cl.compared.length === 1 ? 'names' : 'name'} here (each names only its leading ${noun.many}).`}
    </p>
  );
}

/** The cross-lens summary of one kind. */
export function CrossLensSummary({
  id, level, kind, focus, noun, name, partnerHref, lenses, pair,
}: {
  id: string;
  level: 2 | 3;
  kind: PartnershipKind;
  focus: string;
  noun: { one: string; many: string };
  name: (iso3: string) => string;
  partnerHref?: (iso3: string) => string | null;
  lenses: Readonly<Record<LensId, LensNames>>;
  /** the pair's other economy: one line on where it sits */
  pair?: string | null;
}) {
  const H = level === 3 ? 'h3' : 'h2';
  const cl = crossLens(lenses);
  const line = summaryLine(cl, noun);
  return (
    <section className="card p-4 sm:p-5" aria-labelledby={`${id}-h`} data-testid="cross-lens-summary">
      <span className="eyebrow block">Across the three lenses</span>
      <H id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        Where the three readings agree: {KIND_LABEL[kind].toLowerCase()}, {focus}
      </H>
      <div className="mt-2 space-y-2 text-sm leading-relaxed text-paper-dim">
        {cl.compared.length >= 2 ? (
          <>
            <p data-testid="cross-lens-line">{line}</p>
            <ul className="space-y-1.5" aria-label={`Readings across ${comparedWords(cl.compared)}`}>
              {cl.groups.map((g) => (
                <li key={g.id} data-reading={g.id}>
                  <span className="font-medium text-paper">{g.words}</span>
                  <span className="text-paper-mute"> ({g.keys.length})</span>: <Names keys={g.keys} name={name} href={partnerHref} />
                </li>
              ))}
            </ul>
          </>
        ) : cl.compared.length === 1 ? (
          <p data-testid="cross-lens-line">
            Only {LENS_INFO[cl.compared[0]].title} has {noun.many} here, so there is nothing to compare it with yet
            {lenses[cl.compared[0]].keys.length ? (
              <>
                {': '}
                <Names keys={lenses[cl.compared[0]].keys} name={name} href={partnerHref} />
              </>
            ) : null}
            .
          </p>
        ) : (
          <p data-testid="cross-lens-line">{cl.pending.length ? 'The lenses are loading.' : `No lens has ${noun.many} here.`}</p>
        )}
        {pair && <PairLine cl={cl} pair={pair} name={name} noun={noun} />}
        {cl.pending.length > 0 && cl.compared.length > 0 && <p className="text-xs text-paper-mute">Still loading: {cl.pending.map((k) => LENS_INFO[k].title).join(', ')}.</p>}
        {cl.absent.map((a) => (
          <p key={a.id} className="text-xs text-paper-mute" data-testid={`cross-lens-absent-${a.id}`}>
            {LENS_INFO[a.id].title}: {a.reason ?? 'no data here'}; left out of the comparison.
          </p>
        ))}
        <p className="text-xs text-paper-mute">
          Each lens names its own leading {noun.many} (up to ten where it ranks them); the readings compare names, not values, and the lenses measure different things.
        </p>
      </div>
    </section>
  );
}

/** One lens's panel: its tag, heading and words for the kind, then its content or the one-line empty state. */
export function LensPanel({
  id, lens, level, kind, scenario, spec,
}: {
  id: string;
  lens: LensId;
  level: 2 | 3;
  kind: PartnershipKind;
  scenario: string | null;
  spec: LensPanelSpec;
}) {
  const H = level === 3 ? 'h3' : 'h2';
  const title = lens === 'aib' && scenario ? `${LENS_INFO.aib.title}: ${scenario}` : LENS_INFO[lens].title;
  const has = spec.content !== undefined && spec.content !== null && spec.content !== false;
  return (
    <section className="space-y-4 border-l-2 hairline-strong pl-3 sm:pl-5" aria-labelledby={`${id}-h`} data-lens={lens} data-testid={`lens-${lens}`}>
      <div>
        <LensTag lens={lens} scenario={scenario} />
        <H id={`${id}-h`} className="rule-neis-sm text-lg font-medium text-paper lg:text-xl">
          {title}
        </H>
        <p className="mt-1 max-w-3xl text-sm leading-relaxed text-paper-dim">
          {KIND_LENS_WORDS[kind][lens]} <span className="text-paper-mute">{LENS_INFO[lens].words}</span>
        </p>
        {spec.tools && <div className="mt-3">{spec.tools}</div>}
      </div>
      {has ? (
        spec.content
      ) : (
        <p className="text-sm text-paper-mute" data-testid="lens-empty">
          {spec.empty ?? `No ${LENS_INFO[lens].title} data for this selection.`}
        </p>
      )}
    </section>
  );
}

/** The kind control, the cross-lens summary and the three lens panels. */
export function PartnershipLenses({
  idPrefix, kind, onKind, kindLabel = 'Partnership type', headingLevel, focus, noun, name, partnerHref, scenarioLabel, lenses, footer, pair,
}: PartnershipLensesProps) {
  const uid = useId().replace(/[^a-zA-Z0-9_-]/g, '');
  const base = `${idPrefix}-${uid}`;
  const names = Object.fromEntries(LENS_IDS.map((k) => [k, lenses[k].names])) as Record<LensId, LensNames>;
  return (
    <div className="space-y-8" data-testid="partnership-lenses" data-kind={kind}>
      <Segmented label={kindLabel} value={kind} options={KIND_OPTIONS} onChange={onKind} />
      <CrossLensSummary id={`${base}-sum`} level={headingLevel} kind={kind} focus={focus} noun={noun} name={name} partnerHref={partnerHref} lenses={names} pair={pair} />
      {LENS_IDS.map((k) => (
        <LensPanel key={k} id={`${base}-${k}`} lens={k} level={headingLevel} kind={kind} scenario={scenarioLabel} spec={lenses[k]} />
      ))}
      {footer}
    </div>
  );
}

/** A small card inside a lens panel (an h3 or h4 heading, a line under it, its body). */
export function LensCard({ id, title, sub, level = 3, children, testId }: { id: string; title: string; sub?: ReactNode; level?: 3 | 4; children: ReactNode; testId?: string }) {
  const H = level === 4 ? 'h4' : 'h3';
  return (
    <section className="card p-4 sm:p-5" aria-labelledby={`${id}-h`} data-testid={testId ?? id}>
      <H id={`${id}-h`} className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
        {title}
      </H>
      {sub && <p className="mt-1 text-sm leading-relaxed text-paper-dim">{sub}</p>}
      <div className="mt-3 space-y-2 text-sm text-paper-dim">{children}</div>
    </section>
  );
}

/** A ranked list of economies with a value and a note each (names are links where `href` gives one). */
export function RankedNames<T>({
  rows, label, keyOf, nameOf, href, value, note, max,
}: {
  rows: readonly T[];
  label: string;
  keyOf: (r: T) => string;
  nameOf: (r: T) => string;
  href?: (r: T) => string | null;
  value?: (r: T) => string;
  note?: (r: T) => ReactNode;
  max?: number;
}) {
  const shown = max ? rows.slice(0, max) : rows;
  return (
    <ol className="space-y-1.5" aria-label={label}>
      {shown.map((r, i) => {
        const to = href?.(r) ?? null;
        return (
          <li key={keyOf(r)}>
            <span className="flex flex-wrap items-baseline justify-between gap-x-3">
              <span className="min-w-0">
                <span className="tabular text-paper-mute">{i + 1}. </span>
                {to ? (
                  <Link className="link text-paper" to={to}>
                    {nameOf(r)}
                  </Link>
                ) : (
                  <span className="text-paper">{nameOf(r)}</span>
                )}
              </span>
              {value && <span className="tabular text-xs text-paper-mute">{value(r)}</span>}
            </span>
            {note && <span className="block text-xs text-paper-mute">{note(r)}</span>}
          </li>
        );
      })}
    </ol>
  );
}
