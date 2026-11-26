// "What matters in this supply chain?" (usability pass, 2026-11-25): the answer that opens a technology's
// chain page (/sector/:tech), in plain words, from the observed supplier-share rows of the page's own
// concentration chart (derive/answer.ts chainAnswer): where supply is concentrated (the largest holder at
// each stage), which economies hold the strongest positions (the largest holder of the most rows), the
// most concentrated measure, and what the evidence leaves out. Then one "Affordability & deployment" row:
// which cost and deployment measures the Explorer holds for the technology, each linked to the Advanced
// explorer; it lists evidence and never grades affordability. No row, no claim: "Insufficient evidence".

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { STAGE_LABELS, TECH_LABELS, type StageSlug, type TechSlug } from '../contract.ts';
import { useModel } from '../data/SectorDataProvider.tsx';
import { affordabilityEvidence, chainAnswer, chainAnswerText, licensedWords, MIN_HOLDERS, NO_COST, NO_DEPLOYMENT, yearWords, type ChainLine, type EvidenceRef } from '../derive/answer.ts';
import { CHAIN_ANSWER, CHAIN_QUESTION } from '../copy.ts';
import { lc, listText } from '../format.ts';
import { useSectorUrl } from './state.tsx';

function Row({ term, children, testid }: { term: string; children: ReactNode; testid?: string }) {
  return (
    <div className="grid gap-x-4 gap-y-0.5 border-t hairline py-2 sm:grid-cols-[11rem_minmax(0,1fr)]" data-testid={testid}>
      <dt className="text-xs font-medium uppercase tracking-wide text-paper-mute">{term}</dt>
      <dd className="min-w-0 text-sm text-paper-dim">{children}</dd>
    </div>
  );
}

/** The Advanced explorer, opened on one measure for one technology's cell (or its linked item). */
function useEvidenceHref(): (r: EvidenceRef, tech: TechSlug) => string {
  const { link } = useSectorUrl();
  return (r, tech) =>
    r.v.item_axis === 'cell' ? link(`/sector/explore/${r.v.theme}`, { m: r.v.id, tech: TECH_LABELS[tech], stage: STAGE_LABELS[r.stage] }) : link(`/sector/explore/${r.v.theme}`, { m: r.v.id, item: r.item.slug });
}

/** "Affordability & deployment": the cost and deployment evidence the Explorer holds for a technology. */
export function AffordabilityRow({ tech }: { tech: TechSlug }) {
  const model = useModel();
  const href = useEvidenceHref();
  const a = affordabilityEvidence(model, tech);
  const sep = <span className="text-paper-mute"> · </span>;
  const ref = (r: EvidenceRef, text: string) => (
    <Link key={`${r.v.id}|${r.item.slug}`} className="link" to={href(r, tech)}>
      {text}
    </Link>
  );
  const when = (r: EvidenceRef) => {
    const y = yearWords(r);
    // a measure only one or two economies report is said as such (it is not a comparison)
    const few = r.nGeo > 0 && r.nGeo < MIN_HOLDERS ? `, ${r.nGeo === 1 ? 'one economy' : `${r.nGeo} economies`} only` : '';
    return `${r.v.source.short}${y ? `, ${y}` : ''}${few}`;
  };
  return (
    <Row term={CHAIN_ANSWER.affordability} testid="chain-affordability">
      <span className="block">
        <span className="text-paper">Cost: </span>
        {a.cost.length ? (
          a.cost.map((r, i) => (
            <span key={`${r.v.id}|${r.item.slug}`}>
              {i > 0 && sep}
              {ref(r, r.v.label)} ({when(r)})
            </span>
          ))
        ) : (
          <span>{NO_COST}</span>
        )}
        {a.licensed.length > 0 && <span className="text-paper-mute"> {licensedWords(a.licensed)}</span>}
      </span>
      {a.inputs && (
        <span className="block">
          <span className="text-paper">Input prices: </span>
          {ref(a.inputs.ref, `world prices of ${listText(a.inputs.items.slice(0, 3).map((it) => lc(it.label)))}${a.inputs.items.length > 3 ? ` and ${a.inputs.items.length - 3} more` : ''}`)} ({when(a.inputs.ref)})
        </span>
      )}
      <span className="block">
        <span className="text-paper">Deployment: </span>
        {a.deployment.length ? (
          a.deployment.map((r, i) => (
            <span key={`${r.v.id}|${r.item.slug}`}>
              {i > 0 && sep}
              {ref(r, r.v.short)} ({when(r)})
            </span>
          ))
        ) : (
          <span>{NO_DEPLOYMENT}</span>
        )}
      </span>
    </Row>
  );
}

/** The answer block that opens the chain page. `lines` are the chart's rows; `loading` until the facts are in. */
export function ChainAnswer({ tech, lines, without, loading }: { tech: TechSlug; lines: ChainLine[]; without: StageSlug[]; loading: boolean }) {
  const { link } = useSectorUrl();
  const t = chainAnswerText(chainAnswer(lines, without), tech);
  const stageLink = (s: StageSlug, text: string) => (
    <Link className="link" to={link(`/sector/${tech}/${s}`)}>
      {text}
    </Link>
  );
  return (
    <section aria-labelledby="chain-answer-h" className="panel p-4" data-testid="chain-answer">
      <h2 id="chain-answer-h" className="text-base font-medium text-paper">
        {CHAIN_QUESTION}
      </h2>
      <p className="mt-1 text-xs text-paper-mute">{CHAIN_ANSWER.basis}</p>
      <dl className="mt-2">
        {loading && !lines.length ? (
          <Row term={CHAIN_ANSWER.concentration}>{CHAIN_ANSWER.loading}</Row>
        ) : t.insufficient ? (
          <Row term={CHAIN_ANSWER.concentration} testid="chain-insufficient">
            {t.insufficient}
          </Row>
        ) : (
          <>
            <Row term={CHAIN_ANSWER.concentration} testid="chain-concentration">
              <ul className="space-y-0.5">
                {t.concentration?.map((c) => (
                  <li key={c.stage}>
                    {stageLink(c.stage, STAGE_LABELS[c.stage])}: {c.text}
                  </li>
                ))}
              </ul>
            </Row>
            {t.strongest && (
              <Row term={CHAIN_ANSWER.strongest} testid="chain-strongest">
                {t.strongest}
              </Row>
            )}
            {t.implication && (
              <Row term={CHAIN_ANSWER.implication} testid="chain-implication">
                {t.implication}
              </Row>
            )}
          </>
        )}
        <Row term={CHAIN_ANSWER.limits} testid="chain-limits">
          {loading && !lines.length ? CHAIN_ANSWER.loading : t.limits}
        </Row>
        <AffordabilityRow tech={tech} />
      </dl>
    </section>
  );
}
