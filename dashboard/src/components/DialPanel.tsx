// The dial panel (spec_react.md 8.5): four NEIS sliders with readouts, presets, reset to the NEIS
// baseline (the pipeline's six-stage default weights), and the advanced settings. `layout="wide"` (IA refactor 2026-10-16) lays
// the four dials out in a row for the Country profile's Perspective control; the legacy Strategic
// sidebar keeps the column. Every change writes the URL (drags replace the history entry;
// discrete actions push); releasing a slider flushes the debounced run.
// Copy (spec_country.md C3.5): every control names its index in full (no bare acronyms); each dial
// has an info button with the glossary's plain-language definition.

import { useId, useMemo, type CSSProperties, type KeyboardEvent } from 'react';
import { dialMultiplier, dialReadout, makeParams, presets } from '../engine/params.ts';
import { toRows } from '../engine/table.ts';
import { DIAL_NAMES, type Advanced, type DialName, type Dials } from '../engine/types.ts';
import { useAdvancedChanged, useApp, useRun, useScenario } from '../state/EngineProvider.tsx';
import { toSpec } from '../state/url.ts';
import { fmtMult, TIMES } from '../lib/format.ts';
import { ChangedDot, Disclosure, Segmented, Switch } from './ui.tsx';
import { InfoTip } from './InfoTip.tsx';

export const DIAL_SPECS: { key: DialName; label: string; help: string }[] = [
  { key: 'hawkishness', label: 'Hawkishness', help: 'Security first: weight on exposure and on aligned partners.' },
  { key: 'bullishness', label: 'Bullishness', help: 'Opportunity first: weight on opportunity and export markets.' },
  { key: 'populism', label: 'Populism', help: 'Policy signals and the manufacturing stages (components and assembly).' },
  { key: 'greenness', label: 'Climate', help: 'Climate first: low-emission technologies; at 90 and above coal, oil and gas drop out.' },
];

interface ReadoutRow {
  dial: string;
  parameter: string;
  label: string;
  baseline: number | null;
  current: number | null;
  text: string;
  salient: boolean;
}

/** The dial readout of the current scenario, computed on the main thread (pure, cheap). */
export function useReadout(): ReadoutRow[] {
  const app = useApp();
  const { scenario } = useScenario();
  return useMemo(() => {
    try {
      const params = makeParams(toSpec(scenario), app.bundleLike);
      return toRows(dialReadout(params, app.bundleLike)) as unknown as ReadoutRow[];
    } catch {
      return [];
    }
  }, [scenario, app.bundleLike]);
}

const fillStyle = (v: number, min: number, max: number): CSSProperties => ({ ['--fill' as string]: `${(100 * (v - min)) / (max - min || 1)}%` });

/** PageUp / PageDown move a dial by 25 (arrows step 5 natively, Home / End go to 0 / 100). */
function pageKeys(e: KeyboardEvent<HTMLInputElement>, value: number, set: (v: number) => void) {
  if (e.key === 'PageUp' || e.key === 'PageDown') {
    e.preventDefault();
    set(Math.max(0, Math.min(100, value + (e.key === 'PageUp' ? 25 : -25))));
  }
}

export function DialSlider({
  spec, value, multiplier, rows, onChange, onCommit, fossilNote,
}: {
  spec: (typeof DIAL_SPECS)[number];
  value: number;
  multiplier: number;
  rows: ReadoutRow[];
  onChange: (v: number) => void;
  onCommit: () => void;
  fossilNote?: boolean;
}) {
  const id = `dial-${spec.key}`;
  const moved = value !== 50;
  const valueText = moved ? `${value} · ${fmtMult(multiplier)}` : `${value} · baseline`;
  return (
    <div className="py-3">
      <div className="flex items-baseline justify-between gap-2">
        <span className="inline-flex items-center gap-1">
          <label htmlFor={id} className="label mb-0">
            {spec.label.toUpperCase()}
          </label>
          <InfoTip id={spec.key} term={spec.label} />
        </span>
        <span className={`text-sm tabular ${moved ? 'text-paper' : 'text-paper-mute'}`} aria-hidden="true">
          {valueText}
        </span>
      </div>
      <p id={`${id}-help`} className="mt-1 text-xs leading-snug text-paper-mute">
        {spec.help}
      </p>
      <input
        id={id}
        type="range"
        className="slider mt-1"
        min={0}
        max={100}
        step={5}
        value={value}
        style={fillStyle(value, 0, 100)}
        aria-describedby={`${id}-help`}
        aria-valuetext={moved ? `${value}, weights times ${multiplier.toFixed(2)}` : `${value}, the NEIS baseline`}
        onChange={(e) => onChange(Number(e.target.value))}
        onKeyDown={(e) => pageKeys(e, value, (v) => {
          onChange(v);
        })}
        onKeyUp={onCommit}
        onPointerUp={onCommit}
      />
      <div className="flex justify-between text-[0.6875rem] text-paper-mute" aria-hidden="true">
        <span>{TIMES}0.25</span>
        <span>baseline</span>
        <span>{TIMES}4</span>
      </div>
      {fossilNote && (
        <p className="mt-1.5 flex items-center gap-1.5 text-xs text-paper">
          <span aria-hidden="true">{'⚠'}</span> Coal, oil and gas excluded
        </p>
      )}
      {rows.length > 0 && (
        <ul className="mt-1.5 space-y-0.5 text-xs" aria-label={`What ${spec.label.toLowerCase()} changes`}>
          {rows.map((r) => {
            const changed = r.current !== r.baseline;
            const right = r.text.startsWith(r.label) ? r.text.slice(r.label.length).trim() : r.text;
            const left = r.text.startsWith(r.label) ? r.label : '';
            return (
              <li key={r.parameter} className={`flex justify-between gap-2 ${changed ? 'text-paper' : 'text-paper-mute'}`}>
                <span className="min-w-0">{left}</span>
                <span className="shrink-0 tabular">{right}</span>
              </li>
            );
          })}
        </ul>
      )}
    </div>
  );
}

function WeightSlider({
  label, value, baseline, onChange, onCommit,
}: {
  label: string;
  value: number;
  baseline: number;
  onChange: (v: number) => void;
  onCommit: () => void;
}) {
  const id = useId();
  const changed = value !== baseline;
  return (
    <div className="py-1">
      <div className="flex items-baseline justify-between gap-2 text-xs">
        <label htmlFor={id} className="text-paper-dim">
          {label}
        </label>
        <span className={`tabular ${changed ? 'text-paper' : 'text-paper-mute'}`}>
          {value}
          {changed && <span className="text-paper-mute"> (baseline {baseline})</span>}
        </span>
      </div>
      <input
        id={id}
        type="range"
        className="slider"
        min={0}
        max={10}
        step={0.5}
        value={value}
        style={fillStyle(value, 0, 10)}
        onChange={(e) => onChange(Number(e.target.value))}
        onPointerUp={onCommit}
        onKeyUp={onCommit}
      />
    </div>
  );
}

function CategoryWeights({
  which, title, cats,
}: {
  which: 'es_weights' | 'eo_weights';
  title: string;
  cats: string[];
}) {
  const app = useApp();
  const { scenario, setAdvanced } = useScenario();
  const { flush } = useRun();
  const w = scenario.advanced[which];
  const base = app.urlCtx.defaults[which];
  const changed = cats.some((c) => w[c] !== base[c]);
  const positive = (next: Record<string, number>) => cats.some((c) => next[c] > 0);
  return (
    <Disclosure summary={title} badge={changed ? <ChangedDot /> : undefined}>
      <p className="mb-1 text-xs text-paper-mute">Weights are relative: what counts is each category&rsquo;s share of the total. The score is recomputed exactly from the category scores.</p>
      {changed && !app.manifest.flags?.partner_pillar_consistent && (
        <p className="mb-1 text-xs text-paper">The partnership measures keep the baseline pillars.</p>
      )}
      {cats.map((c) => (
        <WeightSlider
          key={c}
          label={c}
          value={w[c]}
          baseline={base[c]}
          onCommit={flush}
          onChange={(v) =>
            setAdvanced(
              (a) => {
                const next = { ...a[which], [c]: v };
                if (!positive(next)) return a;
                return { ...a, [which]: next };
              },
              { replace: true },
            )
          }
        />
      ))}
      <button type="button" className="btn-quiet mt-1 -ml-2" disabled={!changed} onClick={() => setAdvanced((a) => ({ ...a, [which]: { ...base } }))}>
        Reset
      </button>
    </Disclosure>
  );
}

const DEFAULT_TAU = 0.5;

export function AdvancedPanel() {
  const app = useApp();
  const { scenario, setAdvanced } = useScenario();
  const { flush } = useRun();
  const ch = useAdvancedChanged();
  const a = scenario.advanced;
  const d = app.urlCtx.defaults;
  const set = (f: (a: Advanced) => Advanced, replace = false) => setAdvanced(f, { replace });
  return (
    <details className="disclosure mt-4 border-t hairline" open={ch.any || undefined}>
      <summary className="flex items-center gap-2 py-3">
        <span className="disclosure-chevron text-paper-mute" aria-hidden="true">
          ›
        </span>
        <span className="eyebrow flex-1">Advanced</span>
        {ch.any && <ChangedDot />}
      </summary>
      <CategoryWeights which="es_weights" title="Energy Security: category weights" cats={app.urlCtx.esCategories} />
      <CategoryWeights which="eo_weights" title="Economic Opportunity: category weights" cats={app.urlCtx.eoCategories} />
      <Disclosure summary="Which scores are ranked" badge={ch.sample ? <ChangedDot /> : undefined}>
        <Switch
          label="Apply the rankable rule"
          hint="A score is ranked only when the economy is sovereign and at most this share of the score's weight rests on filled-in data."
          checked={a.tau !== null}
          onChange={(on) => set((x) => ({ ...x, tau: on ? (d.tau ?? DEFAULT_TAU) : null }))}
        />
        <div className="mt-2">
          <div className="flex justify-between text-xs">
            <label htmlFor="adv-tau" className="text-paper-dim">
              Largest filled-in share (&tau;)
            </label>
            <span className="tabular text-paper-mute">{a.tau === null ? 'rule off' : a.tau}</span>
          </div>
          <input
            id="adv-tau"
            type="range"
            className="slider"
            min={0.25}
            max={1}
            step={0.05}
            disabled={a.tau === null}
            value={a.tau ?? d.tau ?? DEFAULT_TAU}
            style={fillStyle(a.tau ?? d.tau ?? DEFAULT_TAU, 0.25, 1)}
            onChange={(e) => {
              const v = Number(e.target.value);
              set((x) => ({ ...x, tau: v }), true);
            }}
            onPointerUp={flush}
            onKeyUp={flush}
          />
        </div>
        {d.tau === null && <p className="mt-1 text-xs text-paper-mute">The stored baseline scores predate the rule, so it is off at the baseline.</p>}
      </Disclosure>
      <Disclosure summary="Fossil technologies" badge={ch.fossil ? <ChangedDot /> : undefined}>
        <Segmented
          label="Coal, oil and gas"
          value={a.fossil}
          options={[
            { value: 'auto', label: 'Auto' },
            { value: 'never', label: 'Never' },
            { value: 'always', label: 'Always' },
          ]}
          onChange={(v) => set((x) => ({ ...x, fossil: v }))}
        />
        <p className="mt-1.5 text-xs text-paper-mute">Auto excludes them at a Climate setting of 90 and above.</p>
      </Disclosure>
      <Disclosure summary="Allied picks" badge={ch.screen ? <ChangedDot /> : undefined}>
        <Switch
          label="Endowment screen"
          hint="An extraction sector is a member's pick only where it has recorded reserves or production of the sector's resources."
          checked={a.require_resource_base}
          onChange={(on) => set((x) => ({ ...x, require_resource_base: on }))}
        />
        <p className="mt-1.5 text-xs text-paper-mute">
          The Strategic Index values do not change; only the allied selection does. The baseline applies the screen{d.require_resource_base ? '' : ' (off in this build)'}.
        </p>
      </Disclosure>
      <Disclosure summary="Partnership Strength viewpoint" badge={ch.psi ? <ChangedDot /> : undefined}>
        <Segmented
          label="Partner-level Partnership Strength averaged over"
          value={a.psi_view}
          options={[
            { value: 'coalition', label: 'Coalition reporters' },
            { value: 'world', label: 'All reporters' },
          ]}
          onChange={(v) => set((x) => ({ ...x, psi_view: v }))}
        />
        <p className="mt-1.5 text-xs text-paper-mute">
          A partner&rsquo;s headline Partnership Strength averages how strong a partner it is for each reporting economy. The baseline averages over {d.psi_view === 'world' ? 'all reporters (world)' : 'the coalition reporters'}.
        </p>
      </Disclosure>
    </details>
  );
}

export function DialPanel({ layout = 'sidebar' }: { layout?: 'sidebar' | 'wide' } = {}) {
  const app = useApp();
  const { scenario, isBaseline, setDial, setDials, reset } = useScenario();
  const { flush } = useRun();
  const rows = useReadout();
  const grid = app.manifest.dial_grid;
  const pr = useMemo(() => presets(), []);
  const presetName = Object.entries(pr).find(([, ds]) => DIAL_NAMES.every((k) => ds[k] === scenario.dials[k]))?.[0] ?? '';
  const bi = app.bundleLike.baselineInputs;
  const fossilOn =
    scenario.advanced.fossil === 'always' || (scenario.advanced.fossil === 'auto' && scenario.dials.greenness >= (bi.fossil_threshold ?? 90));
  const wide = layout === 'wide';
  return (
    <div>
      {!wide && (
        <div className="rule-neis-sm">
          <div className="eyebrow">Your priorities</div>
          <h2 className="mt-1 text-lg font-medium text-paper">Scenario dials</h2>
        </div>
      )}
      <p className="mt-1 text-xs leading-snug text-paper-mute">
        Each dial runs 0&ndash;100; 50 is the NEIS baseline (the pipeline&rsquo;s six-stage default weights). Moving a dial 25 points halves or doubles every weight it owns.
      </p>
      <div className={wide ? 'grid gap-x-6 sm:grid-cols-2 xl:grid-cols-4' : undefined}>
        {DIAL_SPECS.map((s) => (
          <DialSlider
            key={s.key}
            spec={s}
            value={scenario.dials[s.key]}
            multiplier={dialMultiplier(scenario.dials[s.key], grid)}
            rows={rows.filter((r) => r.dial === s.key && r.salient)}
            onChange={(v) => setDial(s.key, v, { replace: true })}
            onCommit={flush}
            fossilNote={s.key === 'greenness' && fossilOn}
          />
        ))}
      </div>
      <div className={wide ? 'mt-2 flex flex-wrap items-end gap-3' : 'mt-2 flex flex-col gap-3'}>
        <div>
          <label htmlFor="preset" className="label">
            Preset
          </label>
          <select
            id="preset"
            className="field field-sm"
            value={presetName}
            onChange={(e) => {
              const p = pr[e.target.value];
              if (p) setDials(p as Dials);
            }}
          >
            {!presetName && <option value="">Custom dials</option>}
            {Object.keys(pr).map((k) => (
              <option key={k} value={k}>
                {k}
              </option>
            ))}
          </select>
        </div>
        <button type="button" className={wide ? 'btn btn-ghost' : 'btn btn-ghost w-full'} disabled={isBaseline} onClick={reset}>
          Reset to NEIS baseline
        </button>
      </div>
      <AdvancedPanel />
    </div>
  );
}
