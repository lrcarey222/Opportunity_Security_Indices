// SectorBar (spec_sector.md 4.0 item 2, 6, 7): the sticky bar under tier 2 on every Sector page.
//
//   * the sector picker (chain, profile and country-in-sector routes only): a technology Select and a
//     stage segmented control whose options carry a coverage glyph (■ rich = 4 or more families,
//     ◧ some, □ thin; in words for screen readers and in the title);
//   * "Highlight a country" (CountrySelect, grouped by the view) with a Clear button;
//   * the EU-27 switch "EU as one bloc", disabled with its reason where nothing is additive;
//   * right-aligned: an aria-live status ("Loading trade data for Batteries, 310 kB") and
//     "Data notes (n)": the notes passed in plus the page's overflow caveats (pageNotes.tsx).
//
// Phones (< 768 px): one summary button ("Batteries · Midstream ▾" or "Filters ▾") and the highlight
// chip; the button opens a BottomSheet with the same controls. The SectorBar never shows a dial.
//
// The bar's height (it wraps: about 76 px at 1280, 105 px at 1024, 145 px at 768) is published as
// `--sector-bar-h` on :root, so sticky elements below it (the profile's section nav) stick under it.

import { useEffect, useId, useRef, useState } from 'react';
import { CountrySelect, type CountryOption } from '../../components/CountrySelect.tsx';
import { Select, Switch, type Option } from '../../components/ui.tsx';
import { useMediaQuery } from '../../shell/useMediaQuery.ts';
import type { StageSlug } from '../contract.ts';
import { BottomSheet } from './BottomSheet.tsx';
import { usePageNotes } from './pageNotes.tsx';

export type StageCoverage = 'rich' | 'some' | 'thin';

export const COVERAGE_GLYPH: Record<StageCoverage, { glyph: string; words: string }> = {
  rich: { glyph: '■', words: 'rich coverage: 4 or more measure families' },
  some: { glyph: '◧', words: 'some coverage' },
  thin: { glyph: '□', words: 'thin coverage: at most one family with country data' },
};

export interface SectorBarStage {
  slug: StageSlug;
  label: string;
  coverage: StageCoverage | null;
  disabled?: boolean;
  /** why it is disabled ("Semiconductors: Midstream only") */
  title?: string;
}

export interface SectorBarPicker {
  tech: string;
  stage: StageSlug | null;
  techOptions: Option[];
  stages: SectorBarStage[];
  onTech: (tech: string) => void;
  onStage: (stage: StageSlug) => void;
}

export interface SectorBarProps {
  /** chain, profile and country-in-sector routes only */
  picker?: SectorBarPicker | null;
  highlight: { value: string | null; options: CountryOption[]; onChange: (iso3: string | null) => void };
  eu: { checked: boolean; onChange: (v: boolean) => void; disabled?: boolean; reason?: string };
  /** the live status line ("Sector data: export 10 Oct 2026", "Loading trade data …") */
  status: string;
  /** manifest notes and view notes; the page's overflow caveats are added from pageNotes */
  notes: readonly string[];
  /** the phone summary ("Batteries · Midstream"); "Filters" when absent */
  summary?: string | null;
}

function StagePicker({ picker, name }: { picker: SectorBarPicker; name: string }) {
  return (
    <fieldset className="min-w-0">
      <legend className="label">Stage</legend>
      <div className="segmented" role="radiogroup" aria-label="Stage">
        {picker.stages.map((s) => {
          const cov = s.coverage ? COVERAGE_GLYPH[s.coverage] : null;
          const title = [s.title, cov ? `${s.label}: ${cov.words}` : null].filter(Boolean).join('. ');
          return (
            <label key={s.slug} title={title || undefined}>
              <input
                type="radio"
                name={name}
                value={s.slug}
                checked={picker.stage === s.slug}
                disabled={s.disabled}
                aria-label={cov ? `${s.label}, ${cov.words}` : s.label}
                onChange={() => picker.onStage(s.slug)}
              />
              {cov && (
                <span aria-hidden="true" className="mr-1.5 text-[0.7rem]">
                  {cov.glyph}
                </span>
              )}
              {s.label}
            </label>
          );
        })}
      </div>
    </fieldset>
  );
}

function Controls({ picker, highlight, eu, idBase }: Pick<SectorBarProps, 'picker' | 'highlight' | 'eu'> & { idBase: string }) {
  const name = highlight.options.find((o) => o.iso3c === highlight.value)?.country ?? highlight.value;
  return (
    <>
      {picker && (
        <>
          <Select label="Technology" value={picker.tech} options={picker.techOptions} onChange={picker.onTech} id={`${idBase}-tech`} className="min-w-[10rem]" />
          <StagePicker picker={picker} name={`${idBase}-stage`} />
        </>
      )}
      <div className="flex items-end gap-1">
        <CountrySelect label="Highlight a country" value={highlight.value ?? ''} options={highlight.options} onChange={(iso) => highlight.onChange(iso)} className="w-56" />
        {highlight.value && (
          <button type="button" className="btn-quiet mb-1" onClick={() => highlight.onChange(null)} aria-label={`Clear the highlight (${name})`}>
            Clear
          </button>
        )}
      </div>
      <div className="pb-1.5">
        <Switch label="EU as one bloc" checked={eu.checked} onChange={eu.onChange} disabled={eu.disabled} hint={eu.disabled ? eu.reason : undefined} />
      </div>
    </>
  );
}

function DataNotes({ notes }: { notes: readonly string[] }) {
  if (!notes.length) return null;
  return (
    <details className="disclosure relative text-xs">
      <summary className="text-paper-mute hover:text-paper">Data notes ({notes.length})</summary>
      <ul className="panel absolute right-0 z-40 mt-2 w-[min(28rem,85vw)] list-disc space-y-1.5 p-4 pl-7 text-paper-dim">
        {notes.map((n) => (
          <li key={n}>{n}</li>
        ))}
      </ul>
    </details>
  );
}

/** Publish an element's height as `--sector-bar-h` on :root while it is mounted. */
function usePublishHeight() {
  const ref = useRef<HTMLDivElement | null>(null);
  useEffect(() => {
    const el = ref.current;
    if (!el || typeof document === 'undefined') return;
    const root = document.documentElement;
    const set = () => root.style.setProperty('--sector-bar-h', `${Math.round(el.getBoundingClientRect().height)}px`);
    set();
    const ro = typeof ResizeObserver !== 'undefined' ? new ResizeObserver(set) : null;
    ro?.observe(el);
    return () => {
      ro?.disconnect();
      root.style.removeProperty('--sector-bar-h');
    };
  }, []);
  return ref;
}

export function SectorBar({ picker, highlight, eu, status, notes, summary }: SectorBarProps) {
  const wide = useMediaQuery('(min-width: 768px)');
  const barRef = usePublishHeight();
  const [sheet, setSheet] = useState(false);
  const idBase = useId();
  const page = usePageNotes();
  const all = [...new Set([...notes, ...page])];
  const hlName = highlight.value ? (highlight.options.find((o) => o.iso3c === highlight.value)?.country ?? highlight.value) : null;
  return (
    <div ref={barRef} className="sticky top-0 z-30 border-b hairline bg-ink-850" data-testid="sector-bar">
      <div className="mx-auto flex max-w-7xl flex-wrap items-end gap-x-5 gap-y-2 px-4 py-2 sm:px-6 lg:px-8">
        {wide ? (
          <Controls picker={picker} highlight={highlight} eu={eu} idBase={idBase} />
        ) : (
          <>
            <button type="button" className="btn btn-ghost btn-sm" aria-haspopup="dialog" aria-expanded={sheet} onClick={() => setSheet(true)}>
              {summary ?? 'Filters'} <span aria-hidden="true">▾</span>
            </button>
            {hlName && (
              <span className="tag">
                {hlName}
                <button type="button" className="ml-1 text-paper-mute hover:text-signal" onClick={() => highlight.onChange(null)} aria-label={`Clear the highlight (${hlName})`}>
                  {'×'}
                </button>
              </span>
            )}
          </>
        )}
        <div className="ml-auto flex flex-wrap items-center gap-x-4 gap-y-1 pb-1">
          <span className="text-xs text-paper-mute" role="status" aria-live="polite">
            {status}
          </span>
          {wide && <DataNotes notes={all} />}
        </div>
      </div>
      {!wide && (
        <BottomSheet open={sheet} onClose={() => setSheet(false)} title={summary ? `${summary}: filters` : 'Filters'}>
          <div className="flex flex-col gap-4">
            <Controls picker={picker} highlight={highlight} eu={eu} idBase={`${idBase}-sheet`} />
            {all.length > 0 && (
              <div>
                <div className="label">Data notes ({all.length})</div>
                <ul className="list-disc space-y-1.5 pl-5 text-xs text-paper-dim">
                  {all.map((n) => (
                    <li key={n}>{n}</li>
                  ))}
                </ul>
              </div>
            )}
          </div>
        </BottomSheet>
      )}
    </div>
  );
}
