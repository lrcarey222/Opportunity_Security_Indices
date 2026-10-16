// About Sectoral Compacts /compacts/about: the method in brief (observed trade network -> shock ->
// existing response -> optimized partnerships -> coordination dividend and frontier -> residual gaps ->
// marginal investment -> emergent network -> compacts -> node treatments), the data year, universe,
// shocks, scenarios and the classification thresholds from the data, and where the full method is.

import { Link } from 'react-router';
import { PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { EYEBROW, PAGE, useAibCtx } from '../AibLayout.tsx';
import { CLASS_INFO } from '../derive/classes.ts';
import { ClassSwatch } from '../charts/ClassChip.tsx';
import { fmtShare } from '../derive/format.ts';
import { ReadingNotes } from './common.tsx';
import { AIB_CLASSES } from '../contract.ts';

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
  { step: 'Shock', text: 'A supplier loses its exports (the headline: China supply removed). The shock exposure is the share of the candidate universe’s demand that disappears.' },
  { step: 'Existing response', text: 'Surviving exporters ship more along the relationships that already exist: what fragmented, uncoordinated action restores.' },
  { step: 'Optimized partnerships', text: 'A linear program reallocates observed exports to restore as much demand as possible, then favours revealed specialization, export opportunity and policy compatibility among equally good solutions.' },
  { step: 'Coordination dividend and frontier', text: 'The dividend is the extra coverage the optimized solution adds over existing relationships; the frontier re-solves with a limit on any one supplier’s share and reports, at each limit, the coverage, the coverage if the solution then also lost its largest supplier (a second shock) and how specialized its suppliers are: what diversification costs and what resilience it buys.' },
  { step: 'Residual gaps', text: 'What even the optimized trade cannot restore: supply that does not exist outside the shocked economy.' },
  { step: 'Marginal investment', text: 'Where a gap remains, a standardized increment of capacity is added at each eligible host in turn (a host must show revealed capability) and the hosts are ranked by the resilience it buys.' },
  { step: 'Emergent network', text: 'The trade and investment relationships of every node together form the allied industrial base network the model finds, not one fixed in advance.' },
  { step: 'Compacts', text: 'Nodes are grouped into a compact only when they are coherent in the supply chain, served by overlapping or complementary partners and in need of similar instruments; a greedy coalition from the demand anchors gives the minimum sufficient membership for each coverage target. A group is a candidate compact only when that coalition lifts the anchors’ coverage clearly above what their existing relationships give them (fragmented action); otherwise it is shown as “compact unnecessary”. The trade compact keeps every economy’s existing-relationship response and lets the coalition’s members also open or expand relationships, so it never falls below existing relationships; it is shown beside the global optimum, in which every economy coordinates.' },
  { step: 'Node treatments', text: 'Each node keeps its own treatment inside a compact: COORDINATE, BUILD, MANAGE SPECIALIZATION, LOW INTERVENTION or INSUFFICIENT DATA.' },
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

export default function AibAboutView() {
  useTitle('About Sectoral Compacts', 'aib');
  const { doc, manifest, shockLabel } = useAibCtx();
  const m = doc.meta;
  const thresholds = Object.entries(m.thresholds);
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow={EYEBROW}
        title="About Sectoral Compacts"
        lede="How the compacts are derived from the observed trade network, what the classes and roles mean, and which data and settings produced the results on these pages."
      />

      <H id="what">What a Sectoral Compact is</H>
      <p className={P}>
        A Sectoral Compact is a variable-geometry coalition that coordinates trade, investment, demand and risk-management policy across a coherent supply chain, where joint action improves
        resilience more than fragmented national or bilateral action would. The module builds no composite score and fixes no alliance in advance: the candidate universe is a setting, and the
        compacts, their members and their roles come out of the model.
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

      <H id="data">Data and settings</H>
      <dl className="grid max-w-3xl gap-x-6 gap-y-2 text-sm sm:grid-cols-[12rem_minmax(0,1fr)]">
        <dt className="text-paper-mute">Trade year</dt>
        <dd className="text-paper-dim">{m.tradeYear}</dd>
        <dt className="text-paper-mute">Candidate universe</dt>
        <dd className="text-paper-dim">{m.universe}</dd>
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
        <dt className="text-paper-mute">Supply scenarios</dt>
        <dd className="text-paper-dim">
          {m.scenarios.map((s) => `${s}${s === m.headlineScenario ? ' (headline)' : ''}`).join(', ')}. Conservative: only the supply the shock frees can move; reallocation: an exporter can
          redirect any of its observed exports toward the universe.
        </dd>
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
