// The six-stage supply chain as a strip (Extraction -> ... -> Recovery & Recycling), with the stage(s) a case is
// about highlighted. Stage names come from the case brief (built from lib/stages.ts STAGE_DISPLAY); the strip
// falls back to STAGE_DISPLAY when a brief carries no stage list. Before the blind answer it locates the case;
// after the reveal the same strip can mark the Explorer's stages (`marked`) and its first stage (`lead`).
// Colour is never the only signal: a highlighted stage also carries a text tag.

import { STAGE_DISPLAY, SUPPLY_STAGES } from '../../../lib/stages.ts';
import type { StageLocation } from '../../model.ts';

export interface StageStripProps {
  location?: StageLocation | null;
  /** The legend read before the strip, e.g. "Where this case sits in the supply chain". */
  label: string;
  /** The tag on a highlighted stage, e.g. "this case" or "covered by this chain". */
  highlightWord?: string;
  /** Post-reveal marks (stage keys): the Explorer's stages and the one it leads with. */
  marked?: readonly string[];
  markedWord?: string;
  lead?: string | null;
  leadWord?: string;
  testid?: string;
}

const DEFAULT_STAGES = SUPPLY_STAGES.map((s) => ({ key: s as string, label: STAGE_DISPLAY[s] }));

export function StageStrip({ location, label, highlightWord = 'this case', marked = [], markedWord = 'needs action', lead = null, leadWord = 'first', testid = 'stage-strip' }: StageStripProps) {
  const stages = location?.stages?.length ? location.stages : DEFAULT_STAGES;
  const hi = new Set(location?.highlight ?? []);
  const mk = new Set(marked);
  return (
    <figure className="m-0" data-testid={testid}>
      <figcaption className="mb-1.5 text-xs font-medium uppercase tracking-wider text-paper-mute">{label}</figcaption>
      <ol className="grid grid-cols-2 gap-1.5 sm:grid-cols-3 lg:grid-cols-6" aria-label={label}>
        {stages.map((s, i) => {
          const on = hi.has(s.key);
          const m = mk.has(s.key);
          const isLead = lead === s.key;
          const tags = [on ? highlightWord : null, m ? markedWord : null, isLead ? leadWord : null].filter(Boolean) as string[];
          return (
            <li
              key={s.key}
              data-stage={s.key}
              data-highlight={on || undefined}
              data-marked={m || undefined}
              aria-current={on && !marked.length ? 'location' : undefined}
              className="panel relative px-2.5 py-2 text-xs"
              style={on || m ? { borderColor: 'var(--color-signal)', color: 'var(--color-paper)' } : { opacity: 0.75 }}
            >
              <span className="block text-paper-mute tabular" aria-hidden="true">
                {i + 1}
                {i < stages.length - 1 ? ' →' : ''}
              </span>
              <span className={on || m ? 'block font-medium text-paper' : 'block text-paper-dim'}>{s.label}</span>
              {tags.length > 0 && <span className="mt-0.5 block text-[0.6875rem] text-signal">{tags.join(' · ')}</span>}
            </li>
          );
        })}
      </ol>
    </figure>
  );
}

export default StageStrip;
