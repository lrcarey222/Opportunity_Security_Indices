// Allied Industrial Base overview /aib (IA refactor, 2026-10-16; brief s.13-14): a light orientation page.
// Where coordination matters (node counts and shares of lost supply by class, the existing class bar; the
// highest-severity nodes and the largest coordination dividends), then one card per remaining question of
// the module (Partnerships, Sectoral Compacts, Country roles). Every number comes from the compacts
// document; none responds to the Perspective (preference) settings.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { PageHead, StatTile } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useAibCtx } from '../AibLayout.tsx';
import { AIB_CLASSES, type AibNode } from '../contract.ts';
import { ClassChip, ClassLegend, ClassSwatch } from '../charts/ClassChip.tsx';
import { LostByClassBar } from '../charts/LostByClassBar.tsx';
import { classInfo } from '../derive/classes.ts';
import { countryRoleRows } from '../derive/countryRoles.ts';
import { fmtPp, fmtShare, plural } from '../derive/format.ts';
import { nodesByClass } from '../derive/nodes.ts';
import { dividendOf, NO_FILTER, pairTradeFlows, topNodes } from '../derive/partnerships.ts';
import { compactHref } from '../nav.ts';
import { Section } from './common.tsx';
import { BasisTag, QuestionCard } from './basis.tsx';

export const AIB_EYEBROW = 'ALLIED INDUSTRIAL BASE';

function NodeList({ nodes, value, label }: { nodes: AibNode[]; value: (n: AibNode) => string; label: string }) {
  return (
    <ol className="divide-y divide-white/5 text-sm" aria-label={label}>
      {nodes.map((n) => (
        <li key={n.id} className="flex flex-wrap items-baseline gap-x-3 gap-y-1 py-2">
          <span className="w-14 shrink-0 text-right font-medium tabular text-paper">{value(n)}</span>
          <span className="min-w-0 flex-1">
            <span className="text-paper">{n.label}</span>{' '}
            <span className="text-xs text-paper-mute">
              {n.techs.join(', ')} · {n.stage}
              {n.compact && (
                <>
                  {' · '}
                  <Link className="link" to={compactHref(n.compact)}>
                    {n.compact}
                  </Link>
                </>
              )}
            </span>
          </span>
          <ClassChip cls={n.cls} />
        </li>
      ))}
    </ol>
  );
}

export default function AibOverviewView() {
  useTitle('Overview', 'aib');
  const { doc, shockLabel, names } = useAibCtx();
  const m = doc.meta;
  const groups = useMemo(() => nodesByClass(doc.nodes), [doc]);
  const lostTotal = groups.reduce((s, g) => s + g.lostUsd, 0);
  const total = doc.nodes.length;
  const byCls = new Map(groups.map((g) => [g.cls, g]));
  const severity = useMemo(() => topNodes(doc.nodes, (n) => n.exposure), [doc]);
  const dividends = useMemo(() => topNodes(doc.nodes, dividendOf), [doc]);
  const trade = useMemo(() => pairTradeFlows(doc, NO_FILTER), [doc]);
  const hostNodes = doc.nodes.filter((n) => n.hosts.length > 0).length;
  const roleRows = useMemo(() => countryRoleRows(doc, names), [doc, names]);
  const types = { candidate: 0, bilateral: 0, unnecessary: 0 };
  for (const c of doc.compacts) {
    if (c.type === 'candidate compact') types.candidate += 1;
    else if (c.type === 'bilateral arrangement sufficient') types.bilateral += 1;
    else types.unnecessary += 1;
  }
  const high = m.thresholds.high_severity;
  const nHigh = typeof high === 'number' ? doc.nodes.filter((n) => (n.exposure ?? 0) >= high).length : null;
  const shown = AIB_CLASSES.filter((c) => c !== 'INSUFFICIENT DATA');
  const insufficient = byCls.get('INSUFFICIENT DATA')?.nodes.length ?? 0;
  const lostShare = (cls: string) => (lostTotal > 0 ? (byCls.get(cls)?.lostUsd ?? 0) / lostTotal : null);
  const biggest = [...groups].filter((g) => g.lostUsd > 0).sort((a, b) => b.lostUsd - a.lostUsd)[0];
  const nCoord = byCls.get('COORDINATE')?.nodes.length ?? 0;
  const rare = nCoord <= Math.max(1, Math.round(total * 0.05));
  const shareLine = groups
    .filter((g) => g.nodes.length)
    .map((g) => `${g.info.label} ${plural(g.nodes.length, 'node')}${lostTotal > 0 ? `, ${fmtShare(g.lostUsd / lostTotal, 0)} of lost supply` : ''}`)
    .join('; ');
  const coordWords = `${plural(nCoord, 'node')} ${nCoord === 1 ? 'is' : 'are'} COORDINATE, where new trade relationships among partners restore most of the gap.`;
  const takeaway =
    total === 0
      ? 'The export carries no classified node.'
      : `${rare ? `Coordination alone is rare in this run: ${coordWords}` : coordWords}${
          biggest ? ` Most of the lost supply (${fmtShare(biggest.lostUsd / lostTotal, 0)}) sits in ${biggest.info.label} nodes: ${biggest.info.meaning.charAt(0).toLowerCase()}${biggest.info.meaning.slice(1)}` : ''
        }`;

  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={AIB_EYEBROW}
        title="Allied Industrial Base"
        lede="Where does international coordination add value, who needs to coordinate, and what form should it take? The model removes a major supplier from the observed trade network and asks how much of the lost supply existing relationships, coordinated trade and new investment can restore, node by node across the energy supply chains."
      >
        <p className="mt-3 max-w-3xl text-sm text-paper-mute">
          Headline: <span className="text-paper-dim">{shockLabel}</span>, {m.headlineScenario} scenario; trade in {m.tradeYear}; {plural(total, 'supply-chain node')}.{' '}
          <Link className="link" to="/compacts/about">
            How the analysis works &rarr;
          </Link>
        </p>
      </PageHead>

      <div className="space-y-14">
        <Section
          id="where"
          title="Where does coordination matter?"
          intro={
            <>
              <BasisTag basis="facts" />
              <p className="mt-2">
                Every supply-chain node (a group of traded goods at one stage) gets one treatment from its own results.{' '}
                {nHigh !== null && typeof high === 'number' ? `${plural(nHigh, 'node')} lose at least ${fmtShare(high, 0)} of their supply to the shock (high severity). ` : ''}
                {takeaway}
              </p>
            </>
          }
        >
          <ul className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4" aria-label="Nodes by treatment">
            {shown.map((cls) => {
              const n = byCls.get(cls)?.nodes.length ?? 0;
              const ls = lostShare(cls);
              return (
                <li key={cls}>
                  <StatTile
                    label={classInfo(cls).label}
                    value={plural(n, 'node')}
                    chip={<ClassSwatch cls={cls} />}
                    sub={`${total ? fmtShare(n / total, 0) : '–'} of nodes${ls !== null ? ` · ${fmtShare(ls, 0)} of lost supply` : ''}`}
                  />
                  <p className="mt-1.5 px-1 text-xs leading-relaxed text-paper-mute">{classInfo(cls).meaning}</p>
                </li>
              );
            })}
          </ul>
          {insufficient > 0 && <p className="text-xs text-paper-mute">{plural(insufficient, 'node')} with too little trade to classify (INSUFFICIENT DATA).</p>}
          <div className="panel p-4">
            <p className="mb-2 text-sm text-paper-dim">Where the lost supply sits, by treatment</p>
            <LostByClassBar groups={groups} label={`Lost supply by class. ${shareLine}`} />
            <div className="mt-2">
              <ClassLegend />
            </div>
          </div>
          <div className="grid gap-6 lg:grid-cols-2">
            <section aria-labelledby="sev-h" className="card p-5">
              <h3 id="sev-h" className="rule-neis-sm text-base font-medium text-paper">
                Highest shock severity
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">Share of each node&apos;s demand the shock removes.</p>
              <NodeList nodes={severity} value={(n) => fmtShare(n.exposure, 0)} label="Nodes with the highest shock exposure" />
            </section>
            <section aria-labelledby="div-h" className="card p-5">
              <h3 id="div-h" className="rule-neis-sm text-base font-medium text-paper">
                Largest coordination dividends
              </h3>
              <p className="mb-2 mt-1 text-sm text-paper-mute">Coverage that coordinated (optimized) trade adds over existing relationships.</p>
              <NodeList nodes={dividends} value={(n) => fmtPp(dividendOf(n), 0)} label="Nodes with the largest coordination dividend" />
            </section>
          </div>
          <p className="text-sm">
            <Link className="link" to="/compacts#map">
              Every node on the strategic map &rarr;
            </Link>
          </p>
        </Section>

        <Section id="questions" title="Who coordinates, and how" intro={<p>Each page answers one question about the emerging Allied Industrial Base.</p>}>
          <ul className="grid gap-4 md:grid-cols-3">
            <QuestionCard
              question="Which trade, investment and development relationships matter?"
              answer={`${plural(trade.length, 'coordinated trade relationship')} across the compacts; eligible investment hosts in ${plural(hostNodes, 'node')}; Friendshore, export-opportunity and development partners.`}
              to="/aib/partnerships"
              cta="Partnerships"
            />
            <QuestionCard
              question="Which supply chains justify a Sectoral Compact, and who belongs?"
              answer={`${plural(types.candidate, 'candidate compact')}${types.bilateral ? `, ${types.bilateral} where a bilateral arrangement is enough` : ''}${types.unnecessary ? `, ${types.unnecessary} where existing relationships do nearly as well` : ''}. Each compact names its nodes, members and what governments should do.`}
              to="/compacts"
              cta="Sectoral Compacts"
            />
            <QuestionCard
              question="What roles do different countries play?"
              answer={`${plural(roleRows.length, 'economy', 'economies')} hold a role in at least one compact: demand anchors, specialized and resource suppliers, investment hosts, technology and bridge partners.`}
              to="/aib/roles"
              cta="Country roles"
            />
          </ul>
          <p className="max-w-3xl text-sm text-paper-mute">
            These are observed and modelled facts: they do not change with the Perspective (preference) settings of a Country profile. Where the Partnerships page ranks partners by
            preference-weighted scores it says so and shows the NEIS baseline.
          </p>
        </Section>
      </div>
    </div>
  );
}
