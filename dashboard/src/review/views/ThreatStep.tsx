// The threat-amelioration questions of a case review (docs/QC_SYSTEM_DESIGN.md 4.4 threat_review), shown in
// the extras step only for cases flagged `threat`, after the reveal. First the case's before -> intervention ->
// after table, read from the snapshot (review/qc/threat.ts built it from the shipped AIB bundle): the threat,
// the intervention, then each measure before and after. Time to capacity is not modelled anywhere in the
// Explorer and every null is shown as "not modelled", never estimated. Then: does it reduce the threat, is the
// scale enough, is the timing plausible, is it commercially plausible, which constraints are missing, what
// success looks like and when to stop. Every question is optional; "I don't know" skips them all.

import { Disclosure } from '../../components/ui.tsx';
import { DontKnowToggle, Likert4Input, MultiCheck, TextArea } from '../components/form.tsx';
import { emptyThreat, type ThreatDraft } from '../flow.ts';
import { DONT_KNOW, type Likert4, type MissingConstraint, type ThreatMetrics, type ThreatSide } from '../model.ts';

export const NOT_MODELLED = 'not modelled';

export const THREAT_QUESTION = {
  reduces_threat: 'Would this intervention reduce the threat?',
  scale_sufficient: 'Is the scale of the response enough for the size of the threat?',
  timing_plausible: 'Could it happen in time to matter?',
  commercially_plausible: 'Is it commercially plausible (would firms invest and buyers switch)?',
  missing_constraints: 'Which constraints does the analysis leave out?',
  success_looks_like: 'What would success look like? (optional)',
  stop_point: 'At what point should governments stop or change course? (optional)',
} as const;

export const MISSING_CONSTRAINT_LABEL: Readonly<Record<MissingConstraint, string>> = {
  qualification: 'Product qualification and certification',
  infrastructure: 'Infrastructure (power, ports, water)',
  workforce: 'Skilled workforce',
  financing: 'Financing',
  demand: 'Assured demand or offtake',
  permitting: 'Permitting',
  other: 'Something else (say what below)',
};

export const DEPENDENCE_OUTCOME_LABEL: Readonly<Record<ThreatMetrics['dependence_outcome'], string>> = {
  diversified: 'Diversified: the largest supplier holds less, below the concentration line',
  shifted: 'Shifted: a different single supplier now holds a dominant share',
  unchanged: 'Unchanged: the same concentration remains',
  unknown: NOT_MODELLED,
};

const pct = (x: number | null | undefined) => (typeof x === 'number' && Number.isFinite(x) ? `${(100 * x).toFixed(1)}%` : NOT_MODELLED);
const num = (x: number | null | undefined, d = 3) => (typeof x === 'number' && Number.isFinite(x) ? x.toFixed(d) : NOT_MODELLED);
const usd = (x: number | null | undefined) => {
  if (typeof x !== 'number' || !Number.isFinite(x)) return NOT_MODELLED;
  const a = Math.abs(x);
  return a >= 1e9 ? `US$${(x / 1e9).toFixed(1)} billion` : a >= 1e6 ? `US$${(x / 1e6).toFixed(1)} million` : `US$${Math.round(x).toLocaleString('en-US')}`;
};
const text = (x: string | number | null | undefined) => (x === null || x === undefined || x === '' ? NOT_MODELLED : String(x));

/** The measure rows of the before/after table: label, before, after. */
export function threatRows(t: ThreatMetrics): [string, string, string][] {
  const side = (f: (s: ThreatSide) => string): [string, string] => [f(t.before), f(t.after)];
  return [
    ['Supplier concentration (Herfindahl-Hirschman index, 0-1)', ...side((s) => num(s.hhi))],
    ['Largest supplier', ...side((s) => text(s.largest_supplier))],
    ["Largest supplier's share", ...side((s) => pct(s.largest_share))],
    ['Top three suppliers’ share', ...side((s) => pct(s.top3_share))],
    ['Share from risky or non-allied sources', ...side((s) => pct(s.risky_share))],
    ['Allied coverage of demand', ...side((s) => pct(s.coverage))],
    ['Credible suppliers', ...side((s) => (s.n_credible_suppliers === null ? NOT_MODELLED : String(s.n_credible_suppliers)))],
    ['Coverage if the largest supplier also fails', ...side((s) => pct(s.n1_coverage))],
  ];
}

/** The single figures of the threat (not before/after). */
export function threatFigures(t: ThreatMetrics): [string, string][] {
  return [
    ['Supply lost to the shock', usd(t.lost_supply_usd)],
    ['Share of the lost supply replaced', pct(t.lost_supply_replaced_share)],
    ['Remaining gap', pct(t.remaining_gap)],
    ['Remaining gap (value)', usd(t.remaining_gap_usd)],
    ['Share met by simulated new capacity', pct(t.new_capacity_share)],
    ['Time to build the new capacity', `${NOT_MODELLED}: the Explorer has no lead-time data`],
    ['What happens to dependence', DEPENDENCE_OUTCOME_LABEL[t.dependence_outcome] ?? NOT_MODELLED],
  ];
}

export function ThreatTable({ t }: { t: ThreatMetrics }) {
  return (
    <div className="space-y-3" data-testid="threat-table">
      <div className="grid gap-3 md:grid-cols-3">
        <div className="panel px-4 py-3">
          <div className="eyebrow">Before: the threat</div>
          <p className="mt-1.5 text-sm text-paper">{text(t.threat)}</p>
        </div>
        <div className="panel px-4 py-3">
          <div className="eyebrow">The intervention</div>
          <p className="mt-1.5 text-sm text-paper">{text(t.intervention)}</p>
        </div>
        <div className="panel px-4 py-3">
          <div className="eyebrow">After</div>
          <p className="mt-1.5 text-sm text-paper">{DEPENDENCE_OUTCOME_LABEL[t.dependence_outcome] ?? NOT_MODELLED}</p>
        </div>
      </div>
      <div className="overflow-x-auto">
        <table className="data-table w-full">
          <caption className="sr-only">Each measure before and after the intervention</caption>
          <thead>
            <tr>
              <th scope="col">Measure</th>
              <th scope="col">Before</th>
              <th scope="col">After</th>
            </tr>
          </thead>
          <tbody>
            {threatRows(t).map(([label, before, after]) => (
              <tr key={label}>
                <td className="text-paper-dim">{label}</td>
                <td className="tabular">{before}</td>
                <td className="tabular">{after}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <dl className="grid gap-x-6 gap-y-1 text-sm sm:grid-cols-2">
        {threatFigures(t).map(([label, v]) => (
          <div key={label} className="flex justify-between gap-3 border-t hairline py-1.5">
            <dt className="text-paper-mute">{label}</dt>
            <dd className="text-right text-paper tabular">{v}</dd>
          </div>
        ))}
      </dl>
      {t.notes.length > 0 && (
        <ul className="list-disc space-y-1 pl-5 text-xs text-paper-mute" aria-label="Notes on these figures">
          {t.notes.map((n) => (
            <li key={n}>{n}</li>
          ))}
        </ul>
      )}
    </div>
  );
}

type LikertKey = 'reduces_threat' | 'scale_sufficient' | 'timing_plausible' | 'commercially_plausible';
const LIKERT_KEYS: readonly LikertKey[] = ['reduces_threat', 'scale_sufficient', 'timing_plausible', 'commercially_plausible'];
/** 'I don't know' on the whole step is recorded as DONT_KNOW in success_looks_like. */
const SKIP_MARK = DONT_KNOW;

export function ThreatStep({ threat, value, onChange }: { threat: ThreatMetrics | null | undefined; value: ThreatDraft | null; onChange: (patch: Partial<ThreatDraft>) => void }) {
  const d = value ?? emptyThreat();
  const skipped = LIKERT_KEYS.every((k) => d[k] === null) && d.success_looks_like === SKIP_MARK;
  return (
    <section aria-labelledby="threat-h" className="space-y-3" data-testid="threat-step">
      <h3 id="threat-h" className="text-lg font-medium text-paper">
        Does the response reduce the threat?
      </h3>
      {threat ? (
        <ThreatTable t={threat} />
      ) : (
        <p className="text-sm text-paper-mute">The before-and-after figures are not available in this release for this case.</p>
      )}
      <p className="max-w-3xl text-sm text-paper-mute">Every question here is optional. Leave a scale blank if you cannot judge it.</p>
      <DontKnowToggle
        checked={skipped}
        onChange={(v) =>
          onChange(v ? { reduces_threat: null, scale_sufficient: null, timing_plausible: null, commercially_plausible: null, missing_constraints: [], success_looks_like: SKIP_MARK } : { success_looks_like: null })
        }
      />
      {!skipped && (
        <div className="space-y-1">
          {LIKERT_KEYS.map((k) => (
            <Likert4Input key={k} legend={THREAT_QUESTION[k]} value={d[k]} onChange={(v: Likert4) => onChange({ [k]: v })} />
          ))}
          <MultiCheck
            legend={THREAT_QUESTION.missing_constraints}
            options={(Object.keys(MISSING_CONSTRAINT_LABEL) as MissingConstraint[]).map((k) => ({ value: k, label: MISSING_CONSTRAINT_LABEL[k] }))}
            value={d.missing_constraints}
            onChange={(v) => onChange({ missing_constraints: v as MissingConstraint[] })}
          />
          {/* the two open questions fold, so the step reads as scales and one checklist (they stay one click away) */}
          <Disclosure summary="Add detail (optional)">
            <TextArea label={THREAT_QUESTION.success_looks_like} rows={2} value={d.success_looks_like ?? ''} onChange={(v) => onChange({ success_looks_like: v })} />
            <TextArea label={THREAT_QUESTION.stop_point} rows={2} value={d.stop_point ?? ''} onChange={(v) => onChange({ stop_point: v })} />
          </Disclosure>
        </div>
      )}
    </section>
  );
}

export default ThreatStep;
