// Shared pieces of the Sectoral Compacts views: the page section (an h2 with its anchor; cards inside use
// h3), the CSV helper (columns in plain words; the shock, scenario and build as the last columns) and the
// notes on reading the results. Inside a QuestionSection (a compact page's four questions, IA refactor
// 2026-10-16) a Section renders its heading as an h3.

import { createContext, useContext, type ReactNode } from 'react';
import { rowsToCsv } from '../../lib/csv.ts';
import type { AibMeta } from '../contract.ts';

const Level = createContext<2 | 3>(2);

export function Section({ id, title, intro, children }: { id: string; title: string; intro?: ReactNode; children: ReactNode }) {
  const sub = useContext(Level) === 3;
  const H = sub ? 'h3' : 'h2';
  return (
    <section id={id} aria-labelledby={`${id}-h`} className="scroll-mt-16">
      <H id={`${id}-h`} className={sub ? 'mb-2 text-lg font-medium text-paper' : 'rule-neis-sm mb-2 text-xl font-medium text-paper lg:text-2xl'}>
        {title}
      </H>
      {intro && <div className="mb-5 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim">{intro}</div>}
      <div className="space-y-6">{children}</div>
    </section>
  );
}

/** A question of a compact page: an h2 section whose inner Sections render as h3. */
export function QuestionSection({ id, title, intro, children }: { id: string; title: string; intro?: ReactNode; children: ReactNode }) {
  return (
    <Section id={id} title={title} intro={intro}>
      <Level.Provider value={3}>{children}</Level.Provider>
    </Section>
  );
}

/** CSV text with the headline shock, scenario and build date as the last three columns. */
export function csvWithRun(rows: Record<string, unknown>[], meta: AibMeta): string {
  return rowsToCsv(rows.map((r) => ({ ...r, shock: meta.headlineShock, scenario: meta.headlineScenario, built: meta.built })));
}

/** How to read the model's results (the compact page and About). */
export function ReadingNotes() {
  return (
    <section aria-labelledby="reading-h" className="panel p-5">
      <h2 id="reading-h" className="text-base font-medium text-paper">
        Reading these results
      </h2>
      <ul className="mt-3 list-disc space-y-2 pl-5 text-sm leading-relaxed text-paper-dim">
        <li>Observed exports are revealed tradable capability, not spare capacity: a country that exported a good can redirect those exports, but the model does not assume it can produce more.</li>
        <li>The investment increment is a standardized synthetic simulation (a fixed share of the node&apos;s demand added at one eligible host), not a project, a cost estimate or a forecast.</li>
        <li>Friendshore never makes an incapable host eligible: a host must show revealed capability in the node or a linked one; partnership scores only order eligible hosts.</li>
        <li>The labels (COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION, INSUFFICIENT DATA), the archetypes and the roles are policy interpretations of continuous results under set thresholds; the numbers behind them are in every table.</li>
      </ul>
    </section>
  );
}
