// About Sectoral Compacts /compacts/about: the method in brief (observed trade network -> demand scope and
// candidate partners -> shock -> existing response -> optimized partnerships -> coordination dividend and
// frontier -> residual gaps -> marginal investment -> emergent network -> compacts -> node treatments),
// the classes; then what the settings mean, with this export's values: demand scope vs candidate-partner
// universe, hold vs release shock semantics, production-constrained vs trade-only supply, the multi-label
// policy needs and the physical-capability warning rule, the exact vs greedy coalition and its tiers,
// true vs instantaneous N-1, the existing-relationship growth bound and the strategic scope; then the
// data, the run settings and the classification thresholds from the data, and where the full method is.

import { Link } from 'react-router';
import { PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import { CLASS_INFO } from '../derive/classes.ts';
import { ClassSwatch } from '../charts/ClassChip.tsx';
import { targetLabel } from '../derive/coalition.ts';
import { fmtShare, listWords, plural } from '../derive/format.ts';
import { candidateUniverse, CONSTRAINT_WORDS, constraintLabel, demandScope, SEMANTICS_WORDS, semanticsLabel, shockSemantics, strategicScope, supplyConstraint } from '../derive/settings.ts';
import { coalitionTargets, isGreedySetMethod, TIER_LABEL, tierMeanings, tierShares } from '../derive/tiers.ts';
import { ReadingNotes } from './common.tsx';
import { AIB_CLASSES, AIB_TIERS, inStrategicScope } from '../contract.ts';

const P = 'mb-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim';

function H({ id, children }: { id: string; children: string }) {
  return (
    <h2 id={id} className="rule-neis-sm mb-3 mt-10 scroll-mt-16 text-xl font-medium text-paper">
      {children}
    </h2>
  );
}

const METHOD: { step: string; text: string }[] = [
  { step: 'Observed trade network', text: 'Bilateral trade in every sub-sector node (a group of HS6 goods at one stage of an energy supply chain), importer declarations first, mirrored where missing.' },
  {
    step: 'Demand scope and candidate partners',
    text: 'Two separate sets: the demand scope is the importers whose lost supply is restored; the candidate partners are the exporters that may add or redirect supply. Protecting one set of economies does not limit who may supply them.',
  },
  {
    step: 'Shock',
    text: 'A supplier loses its exports (the headline: China supply removed). Under hold semantics the shocked economy keeps buying; release (decoupling) is a sensitivity. The shock exposure is the share of the demand scope’s demand that disappears.',
  },
  {
    step: 'Existing response',
    text: 'Surviving exporters ship more along the relationships that already exist, each growing to at most a set multiple of its observed size (2x in the headline): what fragmented, uncoordinated action restores.',
  },
  {
    step: 'Optimized partnerships',
    text: 'A linear program reallocates the candidate partners’ observed exports to restore as much of the demand scope’s demand as possible, then favours revealed specialization, export opportunity and policy compatibility among equally good solutions. Under the production-constrained headline, where credible production or capacity data exist they decide who can supply: an exporter the data list as a producer keeps its full trade response, and one they do not list keeps its current shipments to the demand scope but cannot expand or redirect supply.',
  },
  {
    step: 'Coordination dividend and frontier',
    text: 'The dividend is the extra coverage the optimized solution adds over existing relationships; the frontier re-solves with a limit on any one supplier’s share and reports, at each limit, the coverage, the true N-1 coverage (the largest remaining supplier removed and the trade re-optimized), the instantaneous N-1 retained coverage (coverage × (1 − largest share)) and how specialized its suppliers are: what diversification costs and what resilience it buys.',
  },
  { step: 'Residual gaps', text: 'What even the optimized trade cannot restore: supply that does not exist outside the shocked economy.' },
  { step: 'Marginal investment', text: 'Where a gap remains, a standardized increment of capacity is added at each eligible host in turn (a host must show revealed capability) and the hosts are ranked by the resilience it buys. The new supply goes to the economies that lost it: each is shown as an investor in that host (investor-to-host flows). Each ranked host is a separate simulation of the same increment, so only the best-ranked host counts and the alternatives are never added up.' },
  { step: 'Emergent network', text: 'The trade and investment relationships of every node together form the allied industrial base network the model finds, not one fixed in advance. Each economy’s roles in it (supplier, buyer, investment host, investor) are kept apart and measured by the share of node demand its relationships serve; the Country roles page names a role only at 1 point of node demand or more under the headline shock.' },
  {
    step: 'Compacts',
    text: 'Within the strategic scope, nodes are grouped into a compact only when they are linked in a curated map of physical and process adjacency (one feeds the other within a few links, or both feed the same downstream node; sitting at the same stage is not enough), served by overlapping or complementary partners and in need of similar instruments. The minimum coalition for each coverage target is found exactly, by a binary program (the fewest economies, the demand anchors always in); a greedy heuristic is kept beside it. A group is a candidate compact only when that coalition lifts the anchors’ coverage clearly above what their existing relationships give them (fragmented action); otherwise it is shown as “compact unnecessary”. The trade compact keeps every economy’s existing-relationship response and lets the coalition’s members also open or expand relationships, so it never falls below existing relationships; it is shown beside the global optimum, in which every candidate partner coordinates.',
  },
  {
    step: 'Node treatments',
    text: 'Each node keeps its own headline treatment inside a compact (COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION or INSUFFICIENT DATA) and a profile of policy needs that can combine COORDINATE, BUILD and MANAGE SPECIALIZATION.',
  },
];

/** Plain words for the classification settings (config/aib.yml `classification`); unknown keys print as they are. */
const THRESHOLD_WORDS: Record<string, { label: string; share: boolean }> = {
  low_exposure: { label: 'Shock exposure below which a node is LOW INTERVENTION', share: true },
  adequate_coverage: { label: 'Existing coverage at or above which a node is LOW INTERVENTION', share: true },
  build_gap: { label: 'Residual gap after optimized trade at or above which a node is BUILD', share: true },
  coordinate_dividend: { label: 'Coordination dividend at or above which a node is COORDINATE', share: true },
  manage_largest_share: { label: 'Largest optimized supplier share at or above which a node is MANAGE SPECIALIZATION', share: true },
  manage_top3_share: { label: 'Share of the three largest optimized suppliers at or above which a node is MANAGE SPECIALIZATION', share: true },
  manage_cap: { label: 'Supplier-share limit used to test the cost of diversifying', share: true },
  manage_coverage_loss: { label: 'Coverage lost under that limit at or above which a node is MANAGE SPECIALIZATION', share: true },
  high_severity: { label: 'Shock exposure counted as high severity', share: true },
};

/** "Production-constrained (headline), trade-only". */
const modesWords = (modes: readonly string[] | undefined, headline: string | null | undefined, label: (m: string) => string): string | null =>
  modes && modes.length ? modes.map((x) => `${label(x)}${x === headline ? ' (headline)' : ''}`).join(', ') : null;

export default function AibAboutView() {
  useTitle('About Sectoral Compacts', 'aib');
  const { doc, manifest, shockLabel } = useAibCtx();
  const m = doc.meta;
  const thresholds = Object.entries(m.thresholds);
  const ds = demandScope(m);
  const cu = candidateUniverse(m);
  const sem = shockSemantics(m);
  const con = supplyConstraint(m);
  const st = strategicScope(m);
  const { mid, top } = coalitionTargets(m.targets);
  const nodes = doc.nodes;
  const count = (f: (n: (typeof nodes)[number]) => boolean) => nodes.filter(f).length;
  const phys = { complete: count((n) => n.physicalData === 'complete'), incomplete: count((n) => n.physicalData === 'incomplete'), none: count((n) => n.physicalData === 'none') };
  const hasPhys = nodes.some((n) => n.physicalData !== undefined);
  const nWarned = count((n) => !!n.warning);
  const nSensitive = count((n) => n.coordinateSensitiveToGrowth === true);
  const nOut = count((n) => !inStrategicScope(n));
  const headSem = m.shockSemantics ?? null;
  const headCon = m.supplyConstraint ?? null;
  const tierMeaning = tierMeanings({ mid, top, shares: tierShares(doc.compacts) });
  const nWidest = doc.compacts.filter((c) => isGreedySetMethod(c.coalition.method)).length;
  const nTieBreakOpen = doc.compacts.filter((c) => c.coalition.robustTieBreakSolved === false).length;
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title="About Sectoral Compacts"
        lede="How the compacts are derived from the observed trade network, what the classes, policy needs, warnings, coalition tiers and roles mean, and which data and settings produced the results on these pages."
      />

      <H id="what">What a Sectoral Compact is</H>
      <p className={P}>
        A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand and risk-management policy across a coherent supply chain, where joint action improves
        resilience more than fragmented national or bilateral action would. The module builds no composite score and fixes no alliance in advance: the demand scope and the candidate partners
        are settings, and the compacts, their members and their roles come out of the model.
      </p>

      <H id="method">The method in brief</H>
      <ol className="max-w-3xl space-y-3">
        {METHOD.map((s, i) => (
          <li key={s.step} className="grid grid-cols-[2rem_minmax(0,1fr)] gap-2 text-[0.9375rem] leading-relaxed">
            <span className="tabular text-paper-mute" aria-hidden="true">
              {i + 1}.
            </span>
            <span className="text-paper-dim">
              <strong className="font-medium text-paper">{s.step}.</strong> {s.text}
            </span>
          </li>
        ))}
      </ol>

      <H id="classes">The node classes</H>
      <ul className="max-w-3xl space-y-2 text-[0.9375rem] leading-relaxed">
        {AIB_CLASSES.map((c) => (
          <li key={c} className="grid grid-cols-[1.25rem_minmax(0,1fr)] items-start gap-1">
            <span className="mt-1.5 inline-flex">
              <ClassSwatch cls={c} />
            </span>
            <span className="text-paper-dim">
              <strong className="font-medium text-paper">{CLASS_INFO[c].label}</strong>: {CLASS_INFO[c].meaning}
            </span>
          </li>
        ))}
      </ul>

      <H id="scope">Demand scope and candidate partners</H>
      <p className={P}>
        Two sets define a run. The <strong className="font-medium text-paper">demand scope</strong> is the importers whose lost supply the module restores: demand, the shock exposure and every
        coverage share are measured on the demand scope&apos;s traded demand. The <strong className="font-medium text-paper">candidate partners</strong> are the exporters that may add or redirect
        supply, open or expand relationships, host investment and join coalitions.
      </p>
      <p className={P} data-testid="about-scope">
        {ds || cu ? (
          <>
            In this export the demand scope is {ds ? ds.words : 'not stated'}; the candidate partners are {cu ? `${cu.label.toLowerCase()}: ${cu.words}` : 'not stated'}.
            Restoring every importer&apos;s demand from every exporter (global demand with global partners) is a sensitivity, not the headline: it would count the shortfall of the shocked
            economy&apos;s other customers as the allied base&apos;s problem.
          </>
        ) : (
          <>This export predates the split: one universe ({m.universe}) served as both the demand scope and the candidate partners.</>
        )}
      </p>

      <H id="semantics">Shock semantics: hold and release</H>
      <dl className="grid max-w-3xl gap-x-6 gap-y-2 text-[0.9375rem] leading-relaxed sm:grid-cols-[8rem_minmax(0,1fr)]">
        {(['hold', 'release'] as const).map((k) => (
          <div key={k} className="contents">
            <dt className="font-medium text-paper">
              {SEMANTICS_WORDS[k].label}
              {headSem ? (k === headSem ? ' (headline)' : ' (sensitivity)') : ''}
            </dt>
            <dd className="text-paper-dim">
              {SEMANTICS_WORDS[k].words[0].toUpperCase()}
              {SEMANTICS_WORDS[k].words.slice(1)}.{k === 'hold' ? ' This is the security stress test.' : ' Freed supply makes the shortfall easier to fill.'}
            </dd>
          </div>
        ))}
      </dl>
      <p className={`${P} mt-3`}>Each node&apos;s class under the other semantics is in its sensitivity table on the compact pages.{sem ? '' : ' This export does not state its semantics.'}</p>

      <H id="constraint">Supply constraint: production-constrained and trade-only</H>
      <dl className="grid max-w-3xl gap-x-6 gap-y-2 text-[0.9375rem] leading-relaxed sm:grid-cols-[12rem_minmax(0,1fr)]">
        {(['production_constrained', 'trade_only'] as const).map((k) => (
          <div key={k} className="contents">
            <dt className="font-medium text-paper">
              {CONSTRAINT_WORDS[k].label}
              {headCon ? (k === headCon ? ' (headline)' : ' (sensitivity)') : ''}
            </dt>
            <dd className="text-paper-dim">
              {CONSTRAINT_WORDS[k].words[0].toUpperCase()}
              {CONSTRAINT_WORDS[k].words.slice(1)}.
              {k === 'production_constrained'
                ? ' Physical evidence decides who can supply; it never sets how much in trade value.'
                : ' Observed exports are revealed tradable capability, not spare physical capacity.'}
            </dd>
          </div>
        ))}
      </dl>
      <p className={`${P} mt-3`} data-testid="about-physical-data">
        {hasPhys
          ? `In this export ${plural(phys.complete, 'node has', 'nodes have')} complete physical data, ${phys.incomplete} incomplete and ${phys.none} none; the nodes without complete data keep their trade-only result and their physical validation says so. Each node's class under both modes is on the compact pages and in the node CSVs.`
          : 'This export carries no physical data.'}
      </p>

      <H id="needs">Policy needs and the physical-capability warning</H>
      <p className={P}>
        The headline class is one label per node. Beside it each node carries the policy needs its results show, in any combination: COORDINATE (the coordination dividend reaches its
        threshold), BUILD (the residual gap after optimized trade reaches its threshold) and MANAGE SPECIALIZATION (the optimized supply is concentrated, or diversifying it costs coverage),
        for nodes whose shock exposure is at least {fmtShare(m.thresholds.low_exposure ?? null, 0)}. A node classified LOW INTERVENTION because existing relationships already restore most of its
        demand can still show a COORDINATE need, and a BUILD node can also need specialization managed.
      </p>
      <p className={P} data-testid="about-warning">
        <strong className="font-medium text-paper">PHYSICAL CAPABILITY NOT VALIDATED</strong> marks a LOW INTERVENTION or COORDINATE class that physical data do not back: the node&apos;s physical
        data are incomplete, or physical and trade concentration diverge and either the physical rule was not applied or the class rests on low trade exposure (a dependence that can arrive
        embodied in downstream goods). The warning does not change the class; it is computed for every variant and growth bound too, and shown beside every class shown.
        {nodes.some((n) => n.warning !== undefined) ? ` In this export it marks ${plural(nWarned, 'node')}.` : ''}
      </p>

      <H id="coalitions">Coalitions: exact, greedy and the tiers</H>
      <p className={P}>
        For each coverage target ({listWords(m.targets.map((t) => targetLabel(t)))} of the demand anchors&apos; demand in every node of the compact) the minimum coalition is found exactly: a
        binary program chooses the fewest economies whose coordination reaches the target in every node, the demand anchors always among them. Among coalitions of that minimum size it keeps
        the one with the widest margin over the target in its weakest node, then the one that adds the most supply; where that tie-break hits its time limit the page says so
        {nTieBreakOpen ? ` (${plural(nTieBreakOpen, 'compact', 'compacts')} in this export)` : ''}. A compact&apos;s members are the exact minimum coalition at the {targetLabel(mid)} target. A
        greedy heuristic (adding, one at a time, the economy that most reduces the shortfall) is kept for comparison and draws the &ldquo;coverage as members join&rdquo; chart; it can need
        more economies, or different ones. When no coalition reaches the {targetLabel(mid)} target, the members shown are the widest coalition the greedy path found, and the page says so
        {nWidest ? ` (${plural(nWidest, 'compact', 'compacts')} in this export)` : ''}.
      </p>
      <p className={P}>
        A member&apos;s substitutes are single substitutes: any one of them, added to the rest of the coalition, restores the {targetLabel(mid)} target without that member&apos;s supply. A
        critical supply partner has none; the page says how many other economies together would replace it, or that none would.
      </p>
      <dl className="grid max-w-3xl gap-x-6 gap-y-2 text-[0.9375rem] leading-relaxed sm:grid-cols-[14rem_minmax(0,1fr)]">
        {AIB_TIERS.map((t) => (
          <div key={t} className="contents">
            <dt className="font-medium text-paper">{TIER_LABEL[t]}</dt>
            <dd className="text-paper-dim">{tierMeaning[t]}</dd>
          </div>
        ))}
      </dl>

      <H id="n1">Resilience: true and instantaneous N-1</H>
      <p className={P}>
        Two readings say how much coverage survives if the solution then loses its largest supplier. <strong className="font-medium text-paper">N-1 re-optimized</strong> is the true test: the
        largest remaining supplier is removed with all its shipments and the trade is re-optimized at the same supplier-share limit. <strong className="font-medium text-paper">Instantaneous N-1
        retained coverage</strong> is mechanical: coverage × (1 − the largest supplier&apos;s share), with nothing re-routed. They can differ in either direction: re-optimizing lets other
        suppliers fill in, while the true test also loses the supplier&apos;s existing shipments and keeps the share limit on the rest. The least diversification for resilience (the loosest
        supplier-share limit that keeps coverage at 80% or more) is read on the re-optimized test.
      </p>

      <H id="growth">The existing-relationship growth bound</H>
      <p className={P}>
        Existing relationships cannot grow without limit: in the headline each may grow to at most twice its observed size (2x). Without a bound a small relationship could carry any volume,
        existing relationships would be as flexible as new ones and no coordination dividend could appear. Because the COORDINATE class rests on the gap between existing and optimized coverage,
        each node shows its existing coverage, dividend and class (with its physical-capability warning) with relationships allowed to grow to about 1.25x, 2x, 4x and without bound: on its
        compact&apos;s page, or in place on the landing for a node outside every compact. A node that is COORDINATE under some bounds and another class under others is flagged on the landing
        and on its compact&apos;s page, whatever its headline class: a LOW INTERVENTION node that is COORDINATE only at 1.25x is flagged too, and the flag starts from the headline class.
        {nodes.some((n) => n.coordinateSensitiveToGrowth !== undefined) ? ` In this export ${plural(nSensitive, 'node is', 'nodes are')} flagged.` : ''}
      </p>

      {st && (
        <>
          <H id="strategic">Strategic scope</H>
          <p className={P} data-testid="about-strategic">
            {st.label}: {st.words}.{' '}
            {st.mode !== 'all'
              ? `The other nodes (fossil-fuel supply chains among them) are modelled with the same method and kept in the data, marked outside the scope; they are not headline results, and the node lists leave them out unless asked. In this export ${nOut} of ${nodes.length} nodes are outside the scope.`
              : ''}
          </p>
        </>
      )}

      <H id="data">Data and settings</H>
      <dl className="grid max-w-3xl gap-x-6 gap-y-2 text-sm sm:grid-cols-[12rem_minmax(0,1fr)]">
        <dt className="text-paper-mute">Trade year</dt>
        <dd className="text-paper-dim">{m.tradeYear}</dd>
        {ds || cu ? (
          <>
            <dt className="text-paper-mute">Demand scope</dt>
            <dd className="text-paper-dim">{ds ? `${ds.mode}: ${ds.words}` : '–'}</dd>
            <dt className="text-paper-mute">Candidate partners</dt>
            <dd className="text-paper-dim">{cu ? `${cu.mode}: ${cu.words}` : '–'}</dd>
            <dt className="text-paper-mute">Run label</dt>
            <dd className="text-paper-dim">{m.universe}</dd>
          </>
        ) : (
          <>
            <dt className="text-paper-mute">Candidate universe</dt>
            <dd className="text-paper-dim">{m.universe}</dd>
          </>
        )}
        <dt className="text-paper-mute">Headline shock</dt>
        <dd className="text-paper-dim">{shockLabel}</dd>
        <dt className="text-paper-mute">Shocks modelled</dt>
        <dd className="text-paper-dim">
          <ul className="list-disc pl-5">
            {m.shocks.map((s) => (
              <li key={s.id}>
                {s.label}
                {s.id === m.headlineShock ? ' (headline)' : ''}
              </li>
            ))}
          </ul>
        </dd>
        {sem && (
          <>
            <dt className="text-paper-mute">Shock semantics</dt>
            <dd className="text-paper-dim">{modesWords(m.semantics, m.shockSemantics, semanticsLabel) ?? `${sem.label} (headline)`}</dd>
          </>
        )}
        <dt className="text-paper-mute">Supply scenarios</dt>
        <dd className="text-paper-dim">
          {m.scenarios.map((s) => `${s}${s === m.headlineScenario ? ' (headline)' : ''}`).join(', ')}. Conservative: only the supply the shock frees can move; reallocation: a candidate partner can
          redirect any of its observed exports toward the demand scope.
        </dd>
        {con && (
          <>
            <dt className="text-paper-mute">Supply constraint</dt>
            <dd className="text-paper-dim">{modesWords(m.constraints, m.supplyConstraint, constraintLabel) ?? `${con.label} (headline)`}</dd>
          </>
        )}
        {st && (
          <>
            <dt className="text-paper-mute">Strategic scope</dt>
            <dd className="text-paper-dim">
              {st.label}
              {st.techs.length ? `: ${st.techs.join(', ')}` : ''}
            </dd>
          </>
        )}
        <dt className="text-paper-mute">Coverage targets</dt>
        <dd className="text-paper-dim">{m.targets.map((t) => fmtShare(t, 0)).join(', ')} of the demand anchors&apos; demand</dd>
        <dt className="text-paper-mute">Built</dt>
        <dd className="text-paper-dim">
          {m.built}; exported {manifest.created}
        </dd>
      </dl>

      <h3 className="mb-2 mt-6 text-base font-medium text-paper">Classification thresholds</h3>
      <div className="relative max-w-3xl overflow-x-auto">
        <table className="data-table">
          <caption className="sr-only">The classification thresholds of this export</caption>
          <thead>
            <tr>
              <th scope="col">Setting</th>
              <th scope="col" className="num">
                Value
              </th>
            </tr>
          </thead>
          <tbody>
            {thresholds.map(([k, v]) => {
              const w = THRESHOLD_WORDS[k];
              return (
                <tr key={k}>
                  <td>
                    {w ? w.label : k}
                    <span className="ml-2 font-mono text-xs text-paper-mute">{k}</span>
                  </td>
                  <td className="num tabular">{w?.share ? fmtShare(v, 0) : String(v)}</td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      <H id="more">The full method</H>
      <p className={P}>
        The full method, every setting and its rationale are in <code>docs/aib_methodology.md</code> in the repository, and the settings in <code>config/aib.yml</code>. The data behind these
        pages are written by <code>scripts/84_export_aib_data.R</code> after <code>scripts/31_build_aib_trade_network.R</code> and <code>scripts/32_build_aib_partnerships.R</code>.
      </p>
      <p className={P}>
        <Link className="link" to="/compacts">
          &larr; All compacts
        </Link>
      </p>
      <div className="mt-8">
        <ReadingNotes />
      </div>
    </div>
  );
}
