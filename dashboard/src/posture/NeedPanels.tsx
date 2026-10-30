// The Need section's two panels (spec XII; design 5b), presentational: data in, a node key out (onOpen
// opens the shared drawer, spec XIX). Need is an evidence dimension: its bars are neutral ink, never a
// family colour (spec XX); the choice shows only as its mark.
// A "Where is dependence a problem?": the producing-stage nodes ranked by Secure significance, then
//   supply-chain need (0-100), with reason tags CHOKEPOINT, SYSTEMIC RESILIENCE, CONTROL LAYER and HIGH
//   DEPENDENCE.
// B "Where does the energy system need to move faster?": the technologies ranked by deployment need, with
//   the three families (demand adequacy, target / climate gap, system need), each with its scenario and
//   basis: the scenarios are named, never presented as interchangeable; a climate-aligned value appears only
//   as a labelled alternative. The families come from the economy's detail document (null while it loads:
//   the totals show alone).
// Each panel is a ChartCard with a table twin and a CSV with the posture provenance.

import { useState } from 'react';
import { ChartCard } from '../charts/ChartCard.tsx';
import { TableView, type Col } from '../components/TableView.tsx';
import { Term } from '../components/Term.tsx';
import { POSTURE_TERMS } from './basis.tsx';
import type { PostureCell, PostureCore, PostureDetail } from './contract.ts';
import { postureCsvText, postureProvenance, score100 } from './derive.ts';
import { ratioVsCurrentWords, type Family } from './detail.ts';
import { basisWords, cellMark, NOT_ASSESSED_BORDER, NOT_ASSESSED_HATCH, POSTURE_COLOR, RATIONALE_LABEL, stageDisplay, VERB_LONG, type NeedTag } from './grammar.ts';
import { deploymentNeedRanking, supplyNeedRanking, type DeploymentNeedRow, type SupplyNeedRow } from './sections.ts';

export const NEED_HEADING = 'What problems does this country need to solve?';
export const SUPPLY_NEED_TITLE = 'Where is dependence a problem?';
export const DEPLOYMENT_NEED_TITLE = 'Where does the energy system need to move faster?';

export interface NeedPanelsProps {
  core: PostureCore;
  /** the economy's core cells */
  cells: PostureCell[];
  /** the economy's detail document (deployment families); null while it loads */
  detail: PostureDetail | null;
  /** the economy's name, for the captions and file names */
  name: string;
  /** the selected node ("Tech|Stage"), shown pressed */
  selected?: string | null;
  /** open a node in the shared drawer */
  onOpen: (key: string) => void;
  /** rows of panel A shown before "Show all" (default 10) */
  limit?: number;
  headingLevel?: 2 | 3;
  className?: string;
}

function Mark({ c }: { c: PostureCell | undefined }) {
  if (!c) return null;
  const na = c.posture === 'N';
  return (
    <span
      aria-hidden="true"
      className="inline-flex h-5 w-5 shrink-0 items-center justify-center rounded-[3px] text-[11px] font-semibold text-white"
      style={{ background: na ? NOT_ASSESSED_HATCH : POSTURE_COLOR[c.posture as 'S' | 'C' | 'D'], border: na ? NOT_ASSESSED_BORDER : undefined }}
    >
      {cellMark(c)}
    </span>
  );
}

function Bar({ value, thin = false }: { value: number | null; thin?: boolean }) {
  return (
    <span className={`relative block w-full overflow-hidden rounded-sm bg-white/5 ${thin ? 'h-1' : 'h-2'}`} aria-hidden="true">
      {value !== null && <span className="absolute inset-y-0 left-0 rounded-sm bg-paper-dim/70" style={{ width: `${Math.max(0, Math.min(1, value)) * 100}%` }} />}
    </span>
  );
}

function Tags({ tags }: { tags: NeedTag[] }) {
  if (!tags.length) return null;
  return (
    <span className="flex flex-wrap gap-1" data-testid="need-tags">
      {tags.map((t) => (
        <span key={t} className="rounded-sm border border-white/20 px-1 text-[10px] uppercase tracking-wide text-paper-dim">
          {t}
        </span>
      ))}
    </span>
  );
}

const SUPPLY_COLS: Col<SupplyNeedRow & { rank: number; choice: string }>[] = [
  { id: 'rank', header: 'Rank', value: (r) => r.rank, num: true },
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'stage', header: 'Stage', value: (r) => stageDisplay(r.stage) },
  { id: 'choice', header: 'Choice', value: (r) => r.choice },
  { id: 'need', header: 'Supply-chain need (0-100)', value: (r) => (r.need === null ? null : Math.round(r.need * 100)), num: true },
  { id: 'tags', header: 'Reasons', value: (r) => r.tags.join(', ') || null },
];

function familyLine(f: Family | null | undefined, scenarios: Record<string, string>): string {
  if (!f || f.value === null) return 'not assessed';
  // the climate-aligned alternative is a ratio to current policies, not a 0-100 score
  const ratio = ratioVsCurrentWords(f);
  return [ratio ?? score100(f.value), f.scenario ? (scenarios[f.scenario] ?? f.scenario) : null, ratio ? f.note : null, basisWords(f.basis)].filter(Boolean).join(' · ');
}

type DepRow = DeploymentNeedRow & { choice: string };
const depCols = (scenarios: Record<string, string>): Col<DepRow>[] => [
  { id: 'tech', header: 'Technology', value: (r) => r.tech },
  { id: 'choice', header: 'Choice', value: (r) => r.choice },
  { id: 'need', header: 'Deployment need (0-100)', value: (r) => (r.need === null ? null : Math.round(r.need * 100)), num: true },
  { id: 'demand', header: 'Demand adequacy', value: (r) => (r.families ? familyLine(r.families.demand, scenarios) : null) },
  { id: 'gap', header: 'Target or climate gap', value: (r) => (r.families ? familyLine(r.families.policyGap, scenarios) : null) },
  { id: 'system', header: 'System need', value: (r) => (r.families ? familyLine(r.families.system, scenarios) : null) },
  { id: 'alt', header: 'Climate-aligned alternative', value: (r) => (r.families?.alternative ? familyLine(r.families.alternative, scenarios) : null) },
];

export function NeedPanels({ core, cells, detail, name, selected = null, onOpen, limit = 10, headingLevel = 3, className }: NeedPanelsProps) {
  const [all, setAll] = useState(false);
  const t = core.meta.thresholds;
  const scen = core.meta.scenario_labels;
  const fam = core.meta.family_scenarios;
  const byKey = new Map(cells.map((c) => [`${c.tech}|${c.stage}`, c]));
  const supply = supplyNeedRanking(cells, t);
  const shown = all ? supply : supply.slice(0, limit);
  const dep = deploymentNeedRanking(cells, detail);
  const verbWord = (k: string) => {
    const c = byKey.get(k);
    if (!c) return '—';
    if (c.posture === 'N') return 'Not assessed';
    return c.posture === 'S' ? 'Secure' : c.mode ? VERB_LONG[c.mode] : c.deployMode ? VERB_LONG[c.deployMode] : '—';
  };
  const prov = postureProvenance(core);
  const supplyRows = supply.map((r, i) => ({ ...r, rank: i + 1, choice: verbWord(r.key) }));
  const depRows: DepRow[] = dep.map((r) => ({ ...r, choice: verbWord(r.key) }));
  const scenarioNote = [
    fam.demand ? `demand adequacy reads ${scen[fam.demand] ?? fam.demand}` : null,
    fam.policy_gap ? `the target or climate gap reads ${scen[fam.policy_gap] ?? fam.policy_gap}` : null,
    fam.system ? `system need reads ${scen[fam.system] ?? fam.system}` : null,
  ].filter(Boolean);
  return (
    <div className={`grid gap-4 lg:grid-cols-2 ${className ?? ''}`} data-testid="need-panels">
      <ChartCard
        headingLevel={headingLevel}
        title={SUPPLY_NEED_TITLE}
        sub={
          <>
            The producing-stage nodes where dependence on outside supply is most costly or dangerous: Secure nodes first, then by{' '}
            <Term id={POSTURE_TERMS.supplyChainNeed}>supply-chain need</Term> (0-100).
          </>
        }
        summary={`${name}: ${supply.length} producing-stage nodes ranked by supply-chain need${supply[0] ? `, led by ${supply[0].tech} ${stageDisplay(supply[0].stage)}` : ''}.`}
        table={() => <TableView rows={supplyRows} cols={SUPPLY_COLS} caption={`${name}: supply-chain need by node`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={25} />}
        csv={() => ({
          name: `${name} supply-chain need by node`,
          text: postureCsvText(
            supplyRows.map((r) => ({ rank: r.rank, technology: r.tech, stage: r.stage, stage_name: stageDisplay(r.stage), choice: r.choice, supply_chain_need: r.need, reasons: r.tags.join(';') || null })),
            ['rank', 'technology', 'stage', 'stage_name', 'choice', 'supply_chain_need', 'reasons'],
            prov,
          ),
        })}
        notes={<p>HIGH DEPENDENCE marks a need at or above {score100(t.need)}; the other tags are the Secure rationale. Select a row for the node&apos;s five questions.</p>}
      >
        {supply.length ? (
          <>
            <ol className="space-y-1" data-testid="supply-need-ranking">
              {shown.map((r, i) => {
                const c = byKey.get(r.key);
                const label = `${r.tech} ${stageDisplay(r.stage)}: supply-chain need ${score100(r.need)}; ${verbWord(r.key)}${r.rationale ? `, ${RATIONALE_LABEL[r.rationale].toLowerCase()}` : ''}${r.tags.length ? `; ${r.tags.join(', ')}` : ''}`;
                return (
                  <li key={r.key}>
                    <button
                      type="button"
                      className="grid w-full grid-cols-[1.25rem_minmax(0,1fr)_2.25rem] items-center gap-x-2 gap-y-1 rounded-md px-1.5 py-1 text-left outline-none hover:bg-white/5 focus-visible:ring-2 focus-visible:ring-signal"
                      aria-label={label}
                      aria-pressed={selected === r.key}
                      onClick={() => onOpen(r.key)}
                      data-key={r.key}
                    >
                      <Mark c={c} />
                      <span className="min-w-0 text-sm text-paper-dim">
                        <span className="text-paper-mute">{i + 1}.</span> {r.tech} · {stageDisplay(r.stage)}
                      </span>
                      <span className="text-right text-sm tabular text-paper">{score100(r.need)}</span>
                      <span />
                      <span className="flex flex-col gap-1">
                        <Bar value={r.need} />
                        <Tags tags={r.tags} />
                      </span>
                      <span />
                    </button>
                  </li>
                );
              })}
            </ol>
            {supply.length > limit && (
              <button type="button" className="btn-quiet mt-2" onClick={() => setAll((v) => !v)} aria-expanded={all}>
                {all ? `Show the first ${limit}` : `Show all ${supply.length}`}
              </button>
            )}
          </>
        ) : (
          <p className="text-sm text-paper-mute">No producing-stage node of {name} has a measured supply-chain need.</p>
        )}
      </ChartCard>

      <ChartCard
        headingLevel={headingLevel}
        title={DEPLOYMENT_NEED_TITLE}
        sub={
          <>
            Technologies by <Term id={POSTURE_TERMS.deploymentNeed}>deployment need</Term> (0-100): how much more the energy system needs, from expected demand, the gap to
            national targets and system requirements. This is not supply-chain vulnerability, and it says nothing about manufacturing at home.
          </>
        }
        summary={`${name}: ${dep.length} technologies ranked by deployment need${dep[0] && dep[0].need !== null ? `, led by ${dep[0].tech}` : ''}.`}
        table={() => <TableView rows={depRows} cols={depCols(scen)} caption={`${name}: deployment need by technology`} rowKey={(r) => r.key} onRowClick={(r) => onOpen(r.key)} selectedKey={selected} pageSize={25} />}
        csv={() => ({
          name: `${name} deployment need by technology`,
          text: postureCsvText(
            depRows.map((r) => ({
              technology: r.tech, choice: r.choice, deployment_need: r.need,
              demand_adequacy: r.families?.demand.value ?? null, demand_scenario: r.families?.demand.scenario ?? null, demand_basis: r.families?.demand.basis ?? null,
              target_gap: r.families?.policyGap.value ?? null, target_gap_scenario: r.families?.policyGap.scenario ?? null, target_gap_basis: r.families?.policyGap.basis ?? null,
              system_need: r.families?.system.value ?? null, system_need_basis: r.families?.system.basis ?? null,
              climate_aligned_alternative: r.families?.alternative?.value ?? null, climate_aligned_unit: r.families?.alternative?.unit ?? null,
            })),
            ['technology', 'choice', 'deployment_need', 'demand_adequacy', 'demand_scenario', 'demand_basis', 'target_gap', 'target_gap_scenario', 'target_gap_basis', 'system_need', 'system_need_basis', 'climate_aligned_alternative', 'climate_aligned_unit'],
            prov,
          ),
        })}
        notes={
          <p data-testid="deployment-scenarios">
            {scenarioNote.length ? `Scenarios: ${scenarioNote.join('; ')}. ` : ''}A climate-aligned value, where shown, is an alternative scenario and does not enter the choice.
            Accelerate at {score100(t.deployment)} or more.
          </p>
        }
      >
        {dep.length ? (
          <ol className="space-y-1.5" data-testid="deployment-need-ranking">
            {dep.map((r) => {
              const c = byKey.get(r.key);
              const f = r.families;
              const label = `${r.tech}: deployment need ${score100(r.need)}; ${verbWord(r.key)}${f ? `; demand adequacy ${familyLine(f.demand, scen)}; target or climate gap ${familyLine(f.policyGap, scen)}; system need ${familyLine(f.system, scen)}` : ''}`;
              return (
                <li key={r.key}>
                  <button
                    type="button"
                    className="grid w-full grid-cols-[1.25rem_minmax(0,1fr)_2.25rem] items-center gap-x-2 gap-y-1 rounded-md px-1.5 py-1 text-left outline-none hover:bg-white/5 focus-visible:ring-2 focus-visible:ring-signal"
                    aria-label={label}
                    aria-pressed={selected === r.key}
                    onClick={() => onOpen(r.key)}
                    data-key={r.key}
                  >
                    <Mark c={c} />
                    <span className="min-w-0 text-sm text-paper-dim">{r.tech}</span>
                    <span className="text-right text-sm tabular text-paper">{score100(r.need)}</span>
                    <span />
                    <span className="flex flex-col gap-1">
                      <Bar value={r.need} />
                      {f && (
                        <span className="grid grid-cols-1 gap-x-3 gap-y-0.5 text-[11px] text-paper-mute sm:grid-cols-3" data-testid="deployment-families">
                          <span>Demand: {familyLine(f.demand, scen)}</span>
                          <span>Target gap: {familyLine(f.policyGap, scen)}</span>
                          <span>System: {familyLine(f.system, scen)}</span>
                          {f.alternative && <span className="sm:col-span-3">Alternative, climate-aligned: {familyLine(f.alternative, scen)}</span>}
                        </span>
                      )}
                    </span>
                    <span />
                  </button>
                </li>
              );
            })}
          </ol>
        ) : (
          <p className="text-sm text-paper-mute">No Deployment &amp; Use node of {name} is classified.</p>
        )}
        {!detail && dep.length > 0 && <p className="mt-2 text-xs text-paper-mute">The family breakdown loads with the country&apos;s detail.</p>}
      </ChartCard>
    </div>
  );
}
