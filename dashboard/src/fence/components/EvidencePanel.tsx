// "What the data tell us": the measured evidence for a subject, before any judgment is asked for.
// The scope of every number is explicit: a component's own HS6 sub-component or mineral where one
// exists, and technology-stage evidence labelled as such.

import type { ReactNode } from 'react';
import type { EvidenceSet, Subject } from '../data/adapter.ts';
import type { YFEvidenceRecord, YFReference } from '../data/contract.ts';
import { Disclosure } from '../../components/ui.tsx';
import { EvidenceMetric } from './kit.tsx';

const SCOPE_KIND: Record<string, string> = {
  stage: 'technology-stage basket',
  subcomponent: 'HS6 sub-component',
  mineral: 'mineral',
};

function scopeLabel(recs: YFEvidenceRecord[], fallback: string): string {
  return recs[0]?.scopeLabel ?? fallback;
}

function Block({ title, children }: { title: string; children: ReactNode }) {
  return (
    <section className="panel p-4">
      <h3 className="eyebrow mb-1">{title}</h3>
      {children}
    </section>
  );
}

export function EvidencePanel({ subject, ev, reference, compact }: { subject: Subject; ev: EvidenceSet; reference: YFReference; compact?: boolean }) {
  const isComponent = !!subject.component;
  const primaryIsStage = ev.primary?.scope === 'stage';
  const stageNote = isComponent && !primaryIsStage ? 'whole stage, not this component' : undefined;
  const m = ev.engineMeasure;
  const prim = (ids: string[]) => ids.map((id) => ev.primaryRecords.find((r) => r.metricId === id && (r.measure === m || !m))).filter((r): r is YFEvidenceRecord => !!r);
  const stageRecs = (ids: string[]) => ids.map((id) => ev.stage.find((r) => r.metricId === id && (id !== 'home_share' || r.measure === 'mfg_pipeline'))).filter((r): r is YFEvidenceRecord => !!r);

  const security = prim(['non_coalition_top', 'top_share', 'hhi', 'coalition_share']);
  const minerals = ev.secondary.filter((s) => s.ref.scope === 'mineral').flatMap((s) => s.records.filter((r) => r.measure === 'refining' && (r.metricId === 'non_coalition_top' || r.metricId === 'home_share')));
  const dependency = stageRecs(['home_imports', 'home_net_trade']);
  const opportunity = prim(['home_share']);
  const oppStage = stageRecs(['home_rca', 'home_feasibility', 'home_export_growth', 'home_investment', 'home_investment_growth', 'home_share', 'osi_addressable_market', 'osi_input_cost']);
  const tech = stageRecs(['trl', 'trl_change']);
  const shown = new Set([...security, ...minerals, ...dependency, ...opportunity, ...oppStage, ...tech]);
  const others = [...ev.primaryRecords, ...ev.secondary.flatMap((s) => s.records)].filter((r) => !shown.has(r));
  const conc = (c: string) => reference.concepts[c as keyof YFReference['concepts']]?.label ?? c;

  const scopeLine = ev.primary ? (
    <>
      Measured for <strong className="font-medium text-paper">{scopeLabel(ev.primaryRecords, ev.primary.scopeId)}</strong>
      <span className="text-paper-mute"> ({SCOPE_KIND[ev.primary.scope]}{m ? `; concentration measured on ${reference.measures[m]?.label ?? m}` : ''})</span>
      {isComponent && primaryIsStage && <span className="mt-1 block text-signal">These figures describe the whole technology stage, not this component.</span>}
    </>
  ) : (
    <span className="text-signal">
      No measured scope isolates this component (software, firmware and services are invisible in trade data). The engine therefore has no supplier-share data for it; technology-stage figures below are context only.
    </span>
  );

  return (
    <div className="space-y-4">
      <p className="text-sm leading-relaxed text-paper-dim">{scopeLine}</p>
      <div className={`grid gap-4 ${compact ? '' : 'lg:grid-cols-2'}`}>
        <Block title="Security exposure">
          {security.length === 0 && minerals.length === 0 && dependency.length === 0 && <p className="py-3 text-sm text-paper-mute">No measured security evidence for this scope.</p>}
          {security.map((r) => (
            <EvidenceMetric key={r.metricId + r.measure} r={r} compact={compact} />
          ))}
          {minerals.map((r) => (
            <EvidenceMetric key={`min-${r.scopeId}-${r.metricId}`} r={r} scopeNote={`mineral: ${r.technology}`} compact={compact} />
          ))}
          {dependency.map((r) => (
            <EvidenceMetric key={r.metricId} r={r} scopeNote={stageNote} compact={compact} />
          ))}
        </Block>
        <Block title="Economic opportunity">
          {opportunity.map((r) => (
            <EvidenceMetric key={r.metricId + r.measure} r={r} compact={compact} />
          ))}
          {oppStage.map((r) => (
            <EvidenceMetric key={r.metricId + (r.measure ?? '')} r={r} scopeNote={stageNote} compact={compact} />
          ))}
          {opportunity.length + oppStage.length === 0 && <p className="py-3 text-sm text-paper-mute">No measured opportunity evidence for this scope.</p>}
        </Block>
      </div>
      {tech.length > 0 && (
        <Block title="Technology">
          <div className="grid gap-x-6 sm:grid-cols-2">
            {tech.map((r) => (
              <EvidenceMetric key={r.metricId} r={r} scopeNote={stageNote ?? 'global, stage level'} compact={compact} />
            ))}
          </div>
        </Block>
      )}
      {!compact && others.length > 0 && (
        <Disclosure summary={`Other measures (${others.length})`}>
          <div className="grid gap-x-6 lg:grid-cols-2">
            {others.map((r, i) => (
              <div key={`${r.scopeId}-${r.metricId}-${r.measure}-${i}`}>
                <div className="pt-2 text-xs text-paper-mute">
                  {conc(r.concept)} · {r.scopeLabel}
                </div>
                <EvidenceMetric r={r} />
              </div>
            ))}
          </div>
        </Disclosure>
      )}
    </div>
  );
}
