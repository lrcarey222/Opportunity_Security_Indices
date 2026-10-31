// About Country Insights /countries/about (spec_country.md A4.4): what the module shows and how each
// comparison is made, in plain prose with glossary terms, the memos, the data and its vintage (from the
// manifest), and the module's glossary. Since 2026-10-29 (design 5) it leads with the canonical framework
// (Need -> Prize -> Position -> Choice -> Policy Alignment) and puts the corners and the Strategic Index
// under the Legacy OSI framework, with the conceptual mapping and why the results differ.

import { Link } from 'react-router';
import { GlossaryList } from '../../components/GlossaryList.tsx';
import { Term } from '../../components/Term.tsx';
import { PageHead } from '../../components/ui.tsx';
import { useTitle } from '../../shell/title.ts';
import { PAGE, useCountryCtx } from '../CountryLayout.tsx';
import { COUNTRY_CELL_KEYS, noEsCellsOf } from '../contract.ts';
import { countryStore } from '../data/store.ts';
import { memoDownloadName } from '../derive/model.ts';
import { sectorsByTechText } from '../derive/text.ts';

function H({ id, children }: { id: string; children: string }) {
  return (
    <h2 id={id} className="rule-neis-sm mb-3 mt-10 scroll-mt-16 text-xl font-medium text-paper">
      {children}
    </h2>
  );
}

/** The published memos with their downloads; the plain-text (Markdown) copy is linked from here only (A7.4). */
function MemoList() {
  const { manifest } = useCountryCtx();
  const memos = manifest.countries.filter((c) => c.tier === 'core' && c.memo && !c.memo.stale);
  const revised = manifest.countries.some((c) => c.tier === 'core' && c.memo?.stale);
  if (!memos.length) return <p className="mb-3 max-w-3xl text-sm text-paper-mute">{revised ? 'The memos are being revised. They will be available again once they have been rewritten.' : 'The memos are being prepared.'}</p>;
  const url = (f: string) => countryStore().url(f);
  return (
    <div className="relative mb-3 max-w-4xl overflow-x-auto">
      <table className="data-table">
        <caption className="sr-only">Published memos and their downloads</caption>
        <thead>
          <tr>
            <th scope="col">Country</th>
            <th scope="col">Downloads</th>
            <th scope="col" className="num">
              Words
            </th>
          </tr>
        </thead>
        <tbody>
          {memos.map((c) => {
            const m = c.memo!;
            return (
              <tr key={c.iso3}>
                <td>
                  <Link className="link" to={`/countries/${c.iso3}/memo`}>
                    {c.name}
                  </Link>
                </td>
                <td className="space-x-3 text-sm">
                  {m.pdf && (
                    <a className="link" href={url(m.pdf)} download={memoDownloadName(c.name, manifest.as_of, 'pdf')}>
                      PDF
                    </a>
                  )}
                  {m.docx && (
                    <a className="link" href={url(m.docx)} download={memoDownloadName(c.name, manifest.as_of, 'docx')}>
                      Word
                    </a>
                  )}
                  {m.md && (
                    <a className="link" href={url(m.md)} download={memoDownloadName(c.name, manifest.as_of, 'md')}>
                      Plain text (Markdown)
                    </a>
                  )}
                </td>
                <td className="num tabular">{m.words.toLocaleString('en-US')}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

export default function CountryAboutView() {
  useTitle('Country profiles: data notes', 'country');
  const { manifest } = useCountryCtx();
  const nCore = manifest.counts.core;
  const p = 'mb-3 max-w-3xl text-[0.9375rem] leading-relaxed text-paper-dim';
  return (
    <div className={PAGE}>
      <PageHead
        eyebrow="COUNTRIES · DATA NOTES"
        title="Country profiles: data notes"
        lede="What the country pages show, how each comparison is made, how the memos are written and checked, and where every number comes from."
      />
      <H id="what">What the Country profiles show</H>
      <p className={p}>
        Every Country profile reads the 11 technologies at the six supply-chain stages (extraction, refining and processing, component manufacturing, final assembly and equipment,
        deployment and use, recovery and recycling): 66 technology and stage nodes. It answers, in order: what the country needs, where there is a prize worth pursuing, where it is
        already strong, what it should secure, preserve, catch up in, rely on partners for, deploy faster and deliberately not prioritise, whether its current policy matches those
        choices, and the roles it holds in the allied compacts (the{' '}
        <Link className="link" to="/methods">
          methods
        </Link>{' '}
        say how each step is set). Its sections follow that order: Strategy (the Country Strategy Map, each node coloured by its choice and grouped by fence); the High fence
        (Secure); the Medium fence (Preserve, Catch Up and Partner at the producing stages, with the prize, the competitive position and the partners behind them, and
        Accelerate at Deployment &amp; Use); the Low fence (Watch); Policy Alignment; Allied Strategy (the compacts in which the country holds a role); and Evidence &amp;
        Methodology. One drawer answers the same five questions for any node, from every section.
      </p>

      <H id="strategy">The strategy: need, prize, position, choice, policy alignment</H>
      <p className={p}>
        <strong className="text-paper">Need</strong> comes in two kinds that are never added together. At the five producing stages it is{' '}
        <Term id="supply_chain_need">supply-chain need</Term>: how costly or dangerous dependence on outside supply is (import dependence, supplier concentration, risky supplier exposure,
        trusted alternatives). At Deployment &amp; Use it is <Term id="deployment_need">deployment need</Term>: how much more of the technology the energy system needs (demand adequacy,
        the gap to national targets, system requirements), with its scenario named. <strong className="text-paper">Prize</strong> is{' '}
        <Term id="strategic_industrial_value">Strategic Industrial Value</Term>: whether domestic capability in the node is strategically valuable, from three families, the
        market prize (<Term id="industrial_opportunity">Industrial Opportunity</Term>), industrial commons and future option value, kept apart from current capability and from
        need; a node only the market speaks for is capped below the bar. A capability layer (control software, firmware) is a{' '}
        <Term id="yard">yard</Term> unit of its own, with its own choice; a step whose layers sit in different yards is mixed. A{' '}
        <Term id="large_yard_case">large-yard case</Term> (a mature, passive commodity) is Watch whatever the size of the market.{' '}
        <strong className="text-paper">Position</strong> is <Term id="competitive_position">Competitive Position</Term>, demonstrated capability among the node&apos;s contenders.
      </p>
      <p className={p}>
        <strong className="text-paper">Choice</strong>: the Secure tests come first (operational control, coercive chokepoint, systemic resilience). At Deployment &amp; Use deployment need
        decides Accelerate or Watch. At the producing stages need and prize triage the node and Competitive Position decides the response: Preserve a strong position with a prize, Catch
        Up where the prize and a credible pathway justify it, Partner where the need is high and the domestic case weak, and Watch where the need is low, with a low prize and
        also with a high prize where there is neither a strong position nor a credible pathway (a positive conclusion: domestic industrial priority is low). A node whose need or
        prize is not measured is not assessed, never Watch. <strong className="text-paper">Policy Alignment</strong>{' '}
        then compares observed policy with the choice; policy never feeds the choice. The thresholds are fixed, so nothing on the strategy follows the Perspective dials.
      </p>

      <H id="legacy">The Legacy OSI framework</H>
      <p className={p}>
        The paper baseline&apos;s indices (<Term id="es" />, <Term id="eo" />, the <Term id="si" /> and the <Term id="psi" />) stay at the end of each profile under Evidence &amp;
        Methodology, for replication and comparison; they are not current recommendations. They are always labelled as scores from 0 to 1 that compare the country with other economies;
        raw values always carry their unit, year and source, as the Sector profiles print them. The two are never mixed on one axis. They start on the <Term id="paper_baseline" />: the
        pipeline&apos;s default weights at the six stages. The Perspective control (Explore scenarios) re-weights the legacy priorities, sector rankings and partner scores; the strategy
        and observed and modelled facts (trade, concentration, production, the Allied Industrial Base results) never move.
      </p>
      <p className={p}>
        The four corners map conceptually onto the choices: Grow &asymp; Preserve, Upgrade &asymp; Catch Up, Exposed &asymp; Partner, Stable &asymp; Watch. Results differ because the
        legacy indices mix capability, vulnerability, production, trade, investment, demand, reserves, concentration and deployment, which the strategy measures apart.
      </p>

      <H id="corners">Legacy OSI: reading the four corners</H>
      <p className={p}>
        For each sector (one technology at one supply-chain stage), the country is compared with every economy whose two scores can both be compared in that sector. The chart plots each
        sector at its <Term id="percentile">share of economies beaten</Term>: Economic Opportunity across, Energy Security up. So the axes compare the country with other economies.
      </p>
      <p className={p}>
        The corners then sort the country&apos;s own sectors. They describe; they are not the choices of the strategy.
        The dashed lines sit at the country&apos;s <Term id="quadrant">median sector</Term> on each axis: the middle of its own sectors&apos; positions. A sector is in the <Term id="grow">Grow</Term> corner when both positions are at or above that median (a sector exactly at the median counts as high),{' '}
        <Term id="upgrade">Upgrade</Term> when opportunity is high but energy security low, <Term id="exposed">Exposed</Term> when both are low, and <Term id="stable">Stable</Term> when
        opportunity is low but energy security high. So the corners show where a country is relatively strong or weak, and every country has sectors in more than one corner; the tooltips
        and the table still say whether each score is above or below the sector&apos;s <Term id="median" /> economy. The Security vs opportunity comparison, which shows every
        economy in one sector, splits at the sector&apos;s median economy instead. The RMI (2025) framework associates the corners with export promotion, leapfrogging, friendshoring and
        alternative value creation; the Country profile uses those as descriptions, not instructions. Before 28 October 2026 the Exposed and Stable corners were called
        Secure and Watch, and the memos written before then use those names. No economy has an Energy Security score in {sectorsByTechText(noEsCellsOf(manifest))}, so at most{' '}
        {COUNTRY_CELL_KEYS.length - noEsCellsOf(manifest).length} of the {COUNTRY_CELL_KEYS.length} sectors can be placed.
      </p>

      <H id="alignment">How the strategy is compared with policy</H>
      <p className={p}>
        <Term id="policy_alignment">Policy alignment</Term> sets each assessed node&apos;s choice beside its observed policy (support, protection or localisation, and its momentum
        where the time series allows): under-prioritized when a Secure, Preserve or Catch Up node gets weak support, or an Accelerate node weak deployment policy; over-prioritized when a
        Watch node gets heavy protection; wrong treatment when a Partner node meets localisation; mixed when the sources disagree. There is no overall score, and where policy data are
        thin the alignment says so.
      </p>

      <H id="priorities">Legacy OSI: how priorities are compared with policy</H>
      <p className={p}>
        The Strategic Index adds five parts: 30% economic opportunity, 30% security need (one minus Energy Security), 20% policy activity ranked among the country&apos;s own sectors, 10% the
        supply-chain stage (the two manufacturing stages, components and assembly, count most, then deployment, then extraction, refining and recovery) and 10% the technology&apos;s
        low-emission value. To ask whether current policy matches what the data point to, the policy part is taken out (so policy is not
        counted twice): the remaining <Term id="priority_no_policy">priority without policy</Term> is ranked within the country and cut into thirds, and so is{' '}
        <Term id="policy_index" />. The 3 × 3 matrix shows where the two agree (the diagonal) and where they differ. Policy activity combines <Term id="pams" /> records with, where it rates
        the country, the <Term id="cat" /> read against each technology: a strong climate rating counts for low-emission technologies and against coal, oil and gas. It counts and rates
        policies; it does not measure whether they work.
      </p>
      <p className={p}>
        As context only, the profile also lists the five strongest active industrial-policy measures (not removed by the baseline date) in each sector from the latest completed run of the Global Trade Alert New Industrial Policy
        Observatory ({manifest.nipo?.vintage_label ?? 'the latest completed run'}). A measure&apos;s strength scores its instrument, whether Global Trade Alert judges it to favour domestic
        firms, its breadth and the money or trade it covers, with up to 60% more for packages of several instruments. These measures are not part of the Policy index. Global Trade Alert records
        an EU measure (an Innovation Fund call, the Emissions Trading System, sanctions) under every EU member it covers, so for an EU member many listed measures are EU-wide rather than national:
        the profile marks each one and counts them. Global Trade Alert&apos;s measures are not yet matched to the six supply-chain stages, so until they are the profile counts
        them for the whole economy and does not list them by sector.
      </p>

      <H id="partners">How partners are compared with today&apos;s ties</H>
      <p className={p}>
        The Partnership Strength Index is read from the country&apos;s own viewpoint: how good a partner each other economy is for it as a friendshoring opportunity, where it could invest and source to meet its own security need (<Term id="fs" />), a market (
        <Term id="po" />) and, for middle- and lower-income partners, a place to build supply chains together (<Term id="dp" />). Friendshore is about where investment and sourcing could go, not a ranking of today&apos;s suppliers (the import shares show those): the country&apos;s own need
        is the same for every partner within a sector, so the partner ranking there turns on the partner&apos;s Economic Opportunity, the trade and investment ties, UN-vote
        alignment and ownership. The core-member headline, used where partners are ranked for the coalition as a whole, instead averages the viewpoints of the{' '}
        {nCore} core members; both are named wherever a rank appears.
      </p>
      <p className={p}>
        <Term id="ties_today">Today&apos;s ties</Term> are measured with the reconciled UN Comtrade matrix: a partner&apos;s share of the country&apos;s imports and of its exports in each energy-goods
        basket, averaged over the baskets (never summed across them), plus cross-border factory projects in both directions (<Term id="cmfdi">Cleantech Manufacturing FDI dataset</Term>), shared
        economic-security agreements and <Term id="alignment" />.
      </p>

      <H id="why">Why the &ldquo;why&rdquo; panels say what they say</H>
      <p className={p}>
        Each score is a weighted sum of categories (for Energy Security: energy imports, reserves, production, trade, minerals trade, foreign dependency and more). A bar shows a category&apos;s
        share of the score&apos;s weight times its distance from the middle economy of the sector, so the longest bars are the categories that set the country apart. Under the main bars are the
        figures behind them, in their own units and years, with the share of economies the country beats on each. Measures that are the same for every economy in a sector (such as how
        concentrated world exports are) never count as a reason. Trade partners, factory projects and policy counts are context, not causes: the index&apos;s Energy Security does not measure how
        concentrated a country&apos;s own suppliers are. Two kinds of figure exist only for the EU as a whole: the announced 2035 shares of world manufacturing capacity (International Energy
        Agency, Energy Technology Perspectives) and the electric-vehicle production, sales and trade figures by region (International Energy Agency, Global EV Outlook). The index gives each EU member
        the EU&apos;s value, so for an EU member these figures are marked &ldquo;the EU&apos;s figure, carried by each EU member&rdquo;: they are not the country&apos;s own.
      </p>

      <H id="memos">The memos</H>
      <p className={p}>
        For the {nCore} core coalition members, a short memo summarises the page. An AI writer drafts it from the country&apos;s fact sheet only, and every number, rank, corner and partner name in
        it is a reference to a fact-sheet field that the renderer fills in, so no figure is typed by hand. A machine check then rejects stray numbers, unsupported causal words and missing
        caveats, and a separate AI checker, which did not write it, signs off every claim and every sentence against the fact sheet. Each memo is labelled verbatim: &ldquo;
        {manifest.label}&rdquo; The screen, the PDF and the Word file are rendered from the same document. The memos were written on 14 October 2026, when the corners split each
        sector at its median economy; a memo whose corner statements may differ from today&apos;s chart says so at its top, and its other figures match the current data. The memos
        read the Legacy OSI framework, not the strategy: they are being revised to it.
      </p>
      <MemoList />

      <H id="data">Data and vintage</H>
      <p className={p}>
        NEIS baseline as of {manifest.as_of_label}, at the six supply-chain stages. Built {manifest.created} from Strategic export {(manifest.strategic_bundle_id ?? '–').slice(0, 8)} and Sector export{' '}
        {(manifest.sector_bundle_id ?? '–').slice(0, 8)}; {manifest.counts.sheets} fact sheets for {manifest.counts.economies} economies.
        {manifest.validated ? ' Every shared value was checked against the pipeline outputs and both published exports.' : ' This export was not validated against the published exports.'}
      </p>
      <div className="relative max-w-4xl overflow-x-auto">
        <table className="data-table">
          <caption className="sr-only">Sources of the Country profiles</caption>
          <thead>
            <tr>
              <th scope="col">Source</th>
              <th scope="col">Edition</th>
              <th scope="col">Licence</th>
            </tr>
          </thead>
          <tbody>
            {manifest.sources.map((s) => (
              <tr key={s.id}>
                <td>
                  {s.url ? (
                    <a className="link" href={s.url} target="_blank" rel="noopener noreferrer">
                      {s.label}
                    </a>
                  ) : (
                    s.label
                  )}
                </td>
                <td>{s.edition ?? '–'}</td>
                <td>{s.licence ?? '–'}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      <H id="glossary">Glossary</H>
      <p className={p}>
        The terms used in the Country profiles. Every term of the explorer is on the{' '}
        <Link className="link" to="/glossary">
          glossary page
        </Link>
        .
      </p>
      <GlossaryList module="country" anchors={false} />
    </div>
  );
}
