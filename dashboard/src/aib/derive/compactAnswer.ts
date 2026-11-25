// The first screen of a Strategic Sectoral Compact (Explorer usability pass, 2026-11-25; brief s.2A Compact):
// "What bottleneck does this compact solve, who contributes what, and what gap remains?" in three short lines,
// each read from the compact's own fields under the selected scenario. No new compact logic:
//
//   bottleneck  the spine's BUILD steps (sc.nodes status), largest lost supply first (the same order as the
//               page's "Where supply is lost" list); with none, the steps that lose the most supply and their
//               status; the compact's share of allied demand lost (sc.exposure, sc.lostUsd)
//   who         the core countries (c.core: structural, the same in every scenario) grouped by compact role
//               and step, then the hosts that beat every other host on a BUILD step (sc.buildHosts)
//   gap         what is left after coordinated trade and after simulated new capacity (1 - sc.coverage), with
//               the steps the scenario could not test named as an evidence limit
//
// Missing evidence is said as such ("not assessed", "too little data to test"), never as zero. Pure.

import type { AibStrategicCompact, AibStrategicCompactScenario } from '../contract.ts';
import { fmtShare, fmtUsd, listWords, plural } from './format.ts';
import { spineStatusInfo } from './strategic.ts';

/** One supply-chain step of the spine with its lost supply under the scenario (null: not run). */
export interface AnswerStep {
  id: string;
  label: string;
  status: string | null;
  lostUsd: number | null;
}

export interface CompactAnswer {
  bottleneck: string;
  who: string;
  gap: string;
}

/** At most this many steps are named in the bottleneck line. */
export const ANSWER_STEPS = 2;

const lower = (s: string): string => (/^[A-Z][a-z]{2,}(?:[-\s,&/]|$)/.test(s) ? s.charAt(0).toLowerCase() + s.slice(1) : s);
const cap = (s: string): string => (s ? s[0].toUpperCase() + s.slice(1) : s);
const isNum = (x: unknown): x is number => typeof x === 'number' && Number.isFinite(x);

/** A remaining gap in words: a share, "Less than 1%" for a gap that rounds to nothing, "None" for none. */
export function gapWords(x: number): string {
  if (x <= 1e-9) return 'None';
  return x < 0.005 ? 'Less than 1%' : fmtShare(x, 0);
}

function named(steps: readonly AnswerStep[]): string {
  const shown = steps.slice(0, ANSWER_STEPS).map((x, i) => (i ? lower(x.label) : x.label));
  const more = steps.length - shown.length;
  return more > 0 ? `${shown.join(', ')} and ${plural(more, 'other step')}` : listWords(shown);
}

/**
 * The three lines. `steps`: the spine's steps with their lost supply (the page's node evidence under the
 * scenario); `name`: an economy's name in running text; `managed`: Managed Interdependence (supply has to
 * shift rather than being lost).
 */
export function compactAnswer(
  c: Pick<AibStrategicCompact, 'core'>,
  sc: AibStrategicCompactScenario | null | undefined,
  steps: readonly AnswerStep[],
  scenarioLabel: string,
  name: (iso3: string) => string,
  managed: boolean,
): CompactAnswer {
  const lost = managed ? 'has to shift' : 'is lost';
  const byLost = (xs: AnswerStep[]) => xs.sort((a, b) => (b.lostUsd ?? -1) - (a.lostUsd ?? -1) || a.label.localeCompare(b.label));
  const status = new Map((sc?.nodes ?? []).map((t) => [t[0], t[1]]));
  const withStatus = steps.map((x) => ({ ...x, status: status.get(x.id) ?? x.status }));

  // the bottleneck
  let bottleneck: string;
  if (!sc) bottleneck = `No data for this compact under ${scenarioLabel}.`;
  else {
    const build = byLost(withStatus.filter((x) => x.status === 'BUILD'));
    const hit = byLost(withStatus.filter((x) => isNum(x.lostUsd) && x.lostUsd > 0));
    const share = isNum(sc.exposure) && isNum(sc.lostUsd) ? ` ${fmtShare(sc.exposure, 0)} of the compact’s allied demand (${fmtUsd(sc.lostUsd)}) ${lost} under ${scenarioLabel}.` : '';
    if (build.length) bottleneck = `${named(build)} ${build.length === 1 ? 'needs' : 'need'} new capacity (Build).${share}`;
    else if (hit.length) {
      const top = hit.slice(0, ANSWER_STEPS).map((x) => `${lower(x.label)} (${lower(spineStatusInfo(x.status).label)})`);
      bottleneck = `No step needs new capacity under ${scenarioLabel}; the most supply ${lost} in ${listWords(top)}.${share}`;
    } else bottleneck = `No supply-chain step of this compact loses supply under ${scenarioLabel}.`;
  }

  // who contributes what
  const groups: { role: string; step: string | null; isos: string[] }[] = [];
  for (const e of c.core) {
    const g = groups.find((x) => x.role === e.role && x.step === e.step);
    if (g) g.isos.push(e.iso3);
    else groups.push({ role: e.role, step: e.step, isos: [e.iso3] });
  }
  const parts = groups.map((g) => `${listWords(g.isos.map(name))} (${lower(g.role)}${g.isos.length > 1 ? 's' : ''}${g.step ? `, ${lower(g.step)}` : ''})`);
  const hosts = sc?.buildHosts ?? [];
  let who = parts.length ? `${cap(parts.join('; '))}.` : 'No core countries are named for this compact.';
  if (hosts.length) who += ` New capacity is best placed in ${listWords(hosts.map(name))} under ${scenarioLabel}.`;

  // the remaining gap
  let gap: string;
  const cov = sc?.coverage;
  if (!sc || !cov || !isNum(cov.coordinated)) gap = `Remaining gap: not assessed under ${scenarioLabel}.`;
  else if (isNum(sc.exposure) && sc.exposure === 0) gap = `No allied demand ${lost} under ${scenarioLabel}, so no gap remains.`;
  else if (1 - cov.coordinated <= 1e-9) gap = `All of the compact’s allied demand is met with coordinated trade under ${scenarioLabel}.`;
  else {
    const trade = `${gapWords(1 - cov.coordinated)} of the compact’s allied demand is not met with coordinated trade`;
    const post = isNum(cov.postInvestment) ? gapWords(1 - cov.postInvestment) : null;
    const inv = post ? `, ${post.charAt(0).toLowerCase()}${post.slice(1)} after simulated new capacity` : `; new capacity is not simulated under ${scenarioLabel}`;
    gap = `${trade}${inv}.`;
  }
  const limits: string[] = [];
  const insufficient = withStatus.filter((x) => x.status === 'INSUFFICIENT DATA');
  const untested = withStatus.filter((x) => x.status === 'NOT TESTED');
  if (insufficient.length) limits.push(`${plural(insufficient.length, 'step has', 'steps have')} too little data to test (${listWords(insufficient.map((x) => x.label))})`);
  if (untested.length) limits.push(`${plural(untested.length, 'concentrated step is', 'concentrated steps are')} not tested under ${scenarioLabel}`);
  if (limits.length) gap += ` Evidence limit: ${listWords(limits)}.`;
  return { bottleneck, who, gap };
}
