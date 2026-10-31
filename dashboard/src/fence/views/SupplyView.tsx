// /supply: can trusted diversification solve the problem? Current supply (who holds
// world supply, where the home country buys) against the paper's allied network design for the same
// technology-stage, and three strategies side by side. Reducing vulnerability does not have to mean
// recreating every supply chain at home.

import { useMemo } from 'react';
import { useTitle } from '../title.ts';
import { optionLabel, UNKNOWN } from '../engine/index.ts';
import { stageScopeId } from '../data/adapter.ts';
import type { YFNetworkView, YFSupplier, YFSupplyView } from '../data/contract.ts';
import { useYards } from '../state/YardsContext.tsx';
import { ProvBadge, SectionHead } from '../components/kit.tsx';
import { Disclosure } from '../../components/ui.tsx';
import { PAGE } from '../FenceLayout.tsx';
import { NoAssessment } from './ResultView.tsx';

const TIER: Record<string, string> = { core: 'coalition core', partner: 'coalition partner', none: 'outside the coalition' };
const TIER_COLOR: Record<string, string> = { core: 'var(--color-seq-5)', partner: 'var(--color-seq-3)', none: 'var(--color-div-neg-2)' };
const pct = (x: number) => `${(x * 100).toFixed(x < 0.1 ? 1 : 0)}%`;
const hhiTxt = (h: number) => `HHI ${Math.round(h * 10000).toLocaleString('en-US')}`;

function Bars({ rows, max, extra }: { rows: { key: string; name: string; tier: string; share: number; note?: string }[]; max?: number; extra?: (r: { key: string }) => string | null }) {
  const m = max ?? Math.max(0.01, ...rows.map((r) => r.share));
  return (
    <ul className="space-y-1.5">
      {rows.map((r) => (
        <li key={r.key} className="grid grid-cols-[minmax(0,9rem)_minmax(0,1fr)_3.5rem] items-center gap-2 text-sm">
          <span className="truncate text-paper-dim" title={r.name}>
            {r.name}
          </span>
          <span className="relative h-3 bg-ink-700" aria-hidden="true">
            <span className="yf-grow absolute inset-y-0 left-0" style={{ width: `${(r.share / m) * 100}%`, background: TIER_COLOR[r.tier] ?? TIER_COLOR.core }} />
          </span>
          <span className="tabular text-right text-paper">{pct(r.share)}</span>
          <span className="col-span-3 -mt-1 text-[0.6875rem] text-paper-mute">
            {TIER[r.tier] ?? r.tier}
            {extra?.(r) ? ` · ${extra(r)}` : ''}
          </span>
        </li>
      ))}
    </ul>
  );
}

const sup = (s: YFSupplier[]) => s.map((x) => ({ key: x.geo, name: x.name, tier: x.tier, share: x.share }));

function SupplyCard({ v, title }: { v: YFSupplyView; title: string }) {
  return (
    <section className="panel p-4">
      <div className="flex items-start justify-between gap-2">
        <h3 className="text-sm font-medium text-paper">{title}</h3>
        <ProvBadge p="data" />
      </div>
      <p className="mt-0.5 text-xs text-paper-mute">
        Share of {v.measureLabel}, {v.year} · {v.source}
      </p>
      <div className="mt-3">
        <Bars rows={sup(v.top)} />
      </div>
      <p className="mt-3 text-sm text-paper-dim">
        {hhiTxt(v.hhi)} · like {v.effN.toFixed(1)} equal suppliers · China {pct(v.china)} · coalition {pct(v.core + v.partner)}
      </p>
    </section>
  );
}

function NetworkCard({ n, home }: { n: YFNetworkView; home: string }) {
  return (
    <section className="panel p-4">
      <div className="flex items-start justify-between gap-2">
        <h3 className="text-sm font-medium text-paper">The allied network design for this stage</h3>
        <ProvBadge p="data" />
      </div>
      <p className="mt-0.5 text-xs text-paper-mute">
        Opportunity &amp; Security Indices allied network ({n.method === 'milp' ? 'exact design' : n.method}, diversification weight λ = {n.lambda ?? 'n/a'}, built {n.built}). The EU-27 counts as one supplier. Simulated, not a forecast.
      </p>
      <div className="mt-3">
        <Bars
          rows={n.nodes.map((x) => ({ key: x.node, name: x.label, tier: x.tier, share: x.share }))}
          extra={(r) => {
            const node = n.nodes.find((x) => x.node === r.key);
            return node?.node === home ? 'the home country' : node?.psi != null ? `Partnership Strength with ${home} ${node.psi.toFixed(2)} (rank ${node.psiRank})` : null;
          }}
        />
      </div>
      <p className="mt-3 text-sm text-paper-dim">
        Designed {hhiTxt(n.designedHhi)} (like {n.effectiveProducers.toFixed(1)} equal producers, no supplier above {pct(n.maxShare)})
        {n.observedHhi != null && <> · coalition exports today {hhiTxt(n.observedHhi)}{n.observedTop && <>, largest supplier {n.observedTop.node} {pct(n.observedTop.share)}</>}</>}
        {n.barred.length > 0 && <> · not picked for want of endowment or capacity: {n.barred.join(', ')}</>}
      </p>
    </section>
  );
}

export default function SupplyView() {
  useTitle('Trusted supply');
  const { state, focus, evaluate, data } = useYards();
  const e = useMemo(() => (focus ? evaluate(focus) : null), [focus, evaluate]);
  if (!state.sel || !focus || !e) return <NoAssessment />;
  const b = data.bundle;
  const cell = stageScopeId(focus.technology, focus.stage);
  const stageSupply = b.supply.find((s) => s.scopeId === cell);
  const primary = e.evidence.primary;
  const primSupply = primary && primary.scopeId !== cell ? b.supply.find((s) => s.scopeId === primary.scopeId) : undefined;
  const net = b.network.find((n) => n.cell === cell);
  const a = e.scenario.inputs;
  const v = (id: 'qualificationTime' | 'reconstitutionTime') => (a[id].value === UNKNOWN ? 'unknown' : optionLabel(id, a[id].value as string));
  const premium = a.premium.value === UNKNOWN ? null : Number(a.premium.value);
  const coercive = a.nonAlliedTopShare.value === UNKNOWN ? null : Number(a.nonAlliedTopShare.value);
  const currentHhi = a.hhi.value === UNKNOWN ? null : Number(a.hhi.value);
  const home = b.country.iso3;
  const homeInDesign = net?.homeShare;

  const rows: [string, string, string, string][] = [
    ['Supplier concentration', 'One supplier: HHI 10,000', net ? `${hhiTxt(net.designedHhi)} (designed)` : 'n/a', currentHhi !== null ? `${hhiTxt(currentHhi)} (today)` : 'unknown'],
    ['Supply from outside the coalition', '0%', '0% by design', coercive !== null ? `largest such supplier ${pct(coercive)}${a.nonAlliedTopShare.subject ? ` (${a.nonAlliedTopShare.subject})` : ''}` : 'unknown'],
    ['Time to put in place', `reconstitution: ${v('reconstitutionTime')}`, `qualification: ${v('qualificationTime')}`, 'none: today’s market'],
    ['Cost to buyers', premium !== null ? `↑ about ${premium}% (your premium)` : '↑ (premium unknown)', '↑ some (not estimated)', 'lowest'],
    ['Exposure to coercion', 'low', 'low', coercive !== null && coercive >= 0.5 ? 'high: a coercive supplier holds most supply' : coercive !== null ? 'moderate' : 'unknown'],
    ['Exposure to a single point of failure', 'high: one country’s plants and grid', 'low', currentHhi !== null && currentHhi >= 0.18 ? 'high' : 'moderate'],
    ['Domestic capability built', 'most', homeInDesign ? `some (${home} ${pct(homeInDesign)} of the design)` : 'little', 'least'],
  ];

  const r = e.scenarioResult;
  const fit =
    r.yard === 'indeterminate' || r.yard === 'mixed'
      ? 'The diagnosis is not settled yet, so no strategy follows from it: fill the pivotal gaps first.'
      : r.variant === 'small-input' || r.variant === 'small-both'
        ? 'Your diagnosis is a physical chokepoint. Trusted diversification with stockpiles is the default remedy; domestic production is one tool among several, and self-sufficiency is warranted only where trusted capacity could not be qualified in time.'
        : r.yard === 'small'
          ? 'Your diagnosis is a control-layer problem. Trusted sourcing applies to the control layer; the hardware can come from the lowest-cost reliable supplier.'
          : r.yard === 'medium'
            ? 'Your diagnosis is a capability case. Domestic production and trusted diversification can coexist: the medium fence sets how much capability is built at home, with gates for foreign technology that speeds learning.'
            : 'Your diagnosis is the large yard. Lowest-cost sourcing is the default, with diversification toward trusted producers where supply is concentrated.';
  const STRATEGIES: [string, string][] = [
    ['Domestic self-sufficiency', 'Removes foreign leverage, but concentrates supply in one country’s plants, takes longest and costs most. Justified only when nothing else would survive a cutoff.'],
    ['Trusted diversification', 'Spreads supply across trusted producers, home among them, qualified in advance. Usually removes coercive leverage faster and more cheaply than self-sufficiency.'],
    ['Lowest-cost global sourcing', 'Cheapest and fastest for deployment. Acceptable when no supplier could exert coercive leverage, or the good is easily substituted.'],
  ];

  return (
    <div className={PAGE}>
      <SectionHead eyebrow="Trusted supply" title="Can trusted diversification solve the problem?">
        Reducing a vulnerability does not have to mean rebuilding the whole supply chain at home. The question is which strategy removes the leverage at a cost worth paying.
      </SectionHead>
      <p className="mb-6 text-sm text-paper-mute">For {focus.label}.</p>

      <section aria-label="Three strategies" className="grid gap-4 md:grid-cols-3">
        {STRATEGIES.map(([title, text]) => (
          <article key={title} className="panel p-5">
            <h2 className="text-base font-semibold uppercase tracking-[0.1em] text-paper">{title}</h2>
            <p className="mt-2 text-sm leading-relaxed text-paper-dim">{text}</p>
          </article>
        ))}
      </section>
      <p className="block-interpretation mt-6 max-w-3xl text-[0.9375rem] leading-relaxed">{fit}</p>

      <section aria-labelledby="yf-strat" className="mt-10">
        <SectionHead id="yf-strat" title="How the three compare for this assessment">
          From your answers, with measured figures where they exist; directions where no estimate does.
        </SectionHead>
        <div className="overflow-x-auto">
          <table className="w-full min-w-[40rem] text-sm">
            <thead>
              <tr className="border-b hairline text-left">
                <th scope="col" className="label py-2" />
                <th scope="col" className="label py-2">
                  Domestic self-sufficiency
                </th>
                <th scope="col" className="label py-2">
                  Trusted diversification
                </th>
                <th scope="col" className="label py-2">
                  Lowest-cost global sourcing
                </th>
              </tr>
            </thead>
            <tbody>
              {rows.map(([k, d, t, g]) => (
                <tr key={k} className="border-b hairline align-top last:border-b-0">
                  <th scope="row" className="py-2.5 pr-3 text-left font-normal text-paper-dim">
                    {k}
                  </th>
                  <td className="py-2.5 pr-3 text-paper">{d}</td>
                  <td className="py-2.5 pr-3 text-paper">{t}</td>
                  <td className="py-2.5 text-paper">{g}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </section>

      <div className="mt-10">
        <Disclosure summary="The data behind it: who supplies today, where the country buys, and the allied network design">
          <p className="mb-4 text-sm text-paper-mute">
            Supply data for {primSupply ? (e.evidence.primaryRecords[0]?.scopeLabel ?? primSupply.scopeId) : `${focus.technology} — ${focus.stage}`}
            {focus.component && !primSupply && ' (the whole stage: no measured scope isolates this component)'}.
          </p>
          <div className="grid gap-4 lg:grid-cols-2">
            {primSupply && <SupplyCard v={primSupply} title="Who holds world supply of this component" />}
            {stageSupply && <SupplyCard v={stageSupply} title={`Who holds world supply: ${focus.technology} — ${focus.stage} (whole stage)`} />}
            {stageSupply?.homeImports && (
              <section className="panel p-4">
                <div className="flex items-start justify-between gap-2">
                  <h3 className="text-sm font-medium text-paper">Where {b.country.name} buys (whole stage)</h3>
                  <ProvBadge p="data" />
                </div>
                <p className="mt-0.5 text-xs text-paper-mute">Share of {b.country.name} imports of the HS6 basket, {stageSupply.homeImports.year} · UN Comtrade, reconciled matrix</p>
                <div className="mt-3">
                  <Bars rows={sup(stageSupply.homeImports.sources)} />
                </div>
                <p className="mt-3 text-sm text-paper-dim">Import sources {hhiTxt(stageSupply.homeImports.hhi)}</p>
              </section>
            )}
            {net ? <NetworkCard n={net} home={home} /> : <p className="text-sm text-paper-mute">The allied network has no design for this stage.</p>}
          </div>
        </Disclosure>
      </div>
    </div>
  );
}
