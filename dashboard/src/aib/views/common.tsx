// Shared pieces of the Sectoral Compacts views: the page section (an h2 with its anchor; cards inside use
// h3), the CSV helper (columns in plain words; the run settings and build as the last columns) and the
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

/**
 * CSV text with the run as the last columns: the headline shock and scenario, the shock semantics, the
 * supply constraint and the demand scope / candidate partners where the export states them, and the
 * build date.
 */
export function csvWithRun(rows: Record<string, unknown>[], meta: AibMeta): string {
  const run = {
    shock: meta.headlineShock,
    scenario: meta.headlineScenario,
    ...(meta.shockSemantics ? { shock_semantics: meta.shockSemantics } : {}),
    ...(meta.supplyConstraint ? { supply_constraint: meta.supplyConstraint } : {}),
    ...(meta.demandScope ? { demand_scope: meta.demandScope } : {}),
    ...(meta.candidateUniverse ? { candidate_partners: meta.candidateUniverse } : {}),
    built: meta.built,
  };
  return rowsToCsv(rows.map((r) => ({ ...r, ...run })));
}

/**
 * A page lede that states the page's strategic question first (content/vocabulary.ts MODULE_QUESTION), then
 * what it means: the question in the paper colour, the rest as the lede.
 */
export function QuestionLede({ question, children }: { question: string; children: ReactNode }) {
  return (
    <>
      <strong className="font-medium text-paper" data-testid="page-question">
        {question}
      </strong>{' '}
      {children}
    </>
  );
}

/** How to read the results (the compact page and About). */
export function ReadingNotes() {
  return (
    <section aria-labelledby="reading-h" className="panel p-5">
      <h2 id="reading-h" className="text-base font-medium text-paper">
        Reading these results
      </h2>
      <ul className="mt-3 list-disc space-y-2 pl-5 text-sm leading-relaxed text-paper-dim">
        <li>Exports show what a country can sell, not spare capacity: a country that exported a good can redirect those exports, but the analysis does not assume it can produce more.</li>
        <li>
          Where credible production or capacity data exist they decide who can supply (an exporter they do not list keeps its current shipments but cannot expand or redirect supply);
          elsewhere a result rests on trade alone. A Low intervention or Coordinate trade result that production data do not back carries the warning PHYSICAL CAPABILITY NOT VALIDATED
          (production evidence is weak), under the headline and under every variant shown.
        </li>
        <li>Each supply-chain step has one headline treatment, but it can need several actions, and its treatment can change under the sensitivities shown beside it (trade-only supply, release semantics, other disruptions, other growth bounds).</li>
        <li>The investment increment is a simulation (a fixed share of the step&apos;s demand added at one eligible host, capped at that host&apos;s current output), not a project, a cost estimate or a forecast.</li>
        <li>Friendshore never makes an incapable host eligible: a host must already show capability in the step or a linked one; partnership scores only order eligible hosts.</li>
        <li>The treatments (Coordinate trade, Build capacity, Manage specialization, Low intervention, Not enough evidence), the archetypes and the roles are policy readings of continuous results under set thresholds; the numbers behind them are in every table.</li>
      </ul>
    </section>
  );
}
