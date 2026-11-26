// "Changed since last vintage" (docs/change-layer-architecture.md): the conclusion card's change block for one
// posture cell, mounted in posture/CellDrawer. The headline lines first (recommendation, scores past the threshold,
// evidence, confidence) and whether the method changed; "Why did this change?" opens the previous and current
// assessment, the component differences, the sources behind the moved fields and any accepted Intelligence
// impacts. Renders nothing when the build has no change history.

import { Link } from 'react-router';
import { stageDisplay } from '../lib/stages.ts';
import {
  ATTRIBUTION_LABEL,
  changesForCell,
  choiceLabel,
  detailWindow,
  fmtScore,
  fmtShare,
  latestPair,
  scoreLines,
  vintageDate,
  type ChangeRecord,
  type ChangesBundle,
} from './contract.ts';
import { useChanges } from './store.ts';

const cap = (s: string | null | undefined) => (s ? s.charAt(0).toUpperCase() + s.slice(1) : 'n/a');

function Lines({ c }: { c: ChangeRecord }) {
  const rows: { k: string; v: string; testid: string }[] = [];
  if (c.types.includes('recommendation')) rows.push({ k: 'Recommendation', v: `${choiceLabel(c.before.choice)} → ${choiceLabel(c.after.choice)}`, testid: 'rec' });
  for (const s of scoreLines(c)) rows.push({ k: s.label, v: `${fmtScore(s.from)} → ${fmtScore(s.to)}${s.crossed ? ' (crossed the bar)' : ''}`, testid: s.label.toLowerCase() });
  if (c.types.includes('evidence')) rows.push({ k: 'Evidence', v: `${cap(c.before.evidence)} → ${cap(c.after.evidence)}`, testid: 'evidence' });
  if (c.types.includes('confidence')) rows.push({ k: 'Confidence', v: `${cap(c.before.confidence)} → ${cap(c.after.confidence)}`, testid: 'confidence' });
  if (c.types.includes('data') && !rows.length) rows.push({ k: 'Export share', v: `${fmtShare(c.before.share)} → ${fmtShare(c.after.share)}`, testid: 'share' });
  return (
    <dl className="mt-2 grid grid-cols-[auto_1fr] gap-x-4 gap-y-1 text-sm">
      {rows.map((r) => (
        <div key={r.k} className="contents" data-testid={`change-line-${r.testid}`}>
          <dt className="text-paper-mute">{r.k}</dt>
          <dd className="text-paper">{r.v}</dd>
        </div>
      ))}
    </dl>
  );
}

function SideTable({ c }: { c: ChangeRecord }) {
  const rows: [string, string, string][] = [
    ['Choice', choiceLabel(c.before.choice), choiceLabel(c.after.choice)],
    [c.after.needType === 'deployment' ? 'Deployment need' : 'Need', fmtScore(c.before.need), fmtScore(c.after.need)],
    ['Prize', fmtScore(c.before.prize), fmtScore(c.after.prize)],
    ['Position', fmtScore(c.before.cp), fmtScore(c.after.cp)],
    ['Market size', fmtScore(c.before.market), fmtScore(c.after.market)],
    ['Export share', fmtShare(c.before.share), fmtShare(c.after.share)],
    ['Evidence', cap(c.before.evidence), cap(c.after.evidence)],
    ['Confidence', cap(c.before.confidence), cap(c.after.confidence)],
  ];
  return (
    <table className="mt-2 w-full text-xs" data-testid="change-side-table">
      <thead>
        <tr className="text-paper-mute">
          <th className="py-1 text-left font-normal" />
          <th className="py-1 text-left font-normal">Previous</th>
          <th className="py-1 text-left font-normal">Current</th>
        </tr>
      </thead>
      <tbody>
        {rows.map(([k, a, b]) => (
          <tr key={k} className={a !== b ? 'text-paper' : 'text-paper-dim'}>
            <td className="py-0.5 pr-2 text-paper-mute">{k}</td>
            <td className="py-0.5 pr-2">{a}</td>
            <td className="py-0.5">{b}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}

export function ChangeDetail({ c, b }: { c: ChangeRecord; b: ChangesBundle }) {
  const pair = b.pairs.find((p) => p.prev === c.prev && p.cur === c.cur);
  const impacts = b.impacts.filter((i) => c.impacts.includes(i.impact_id));
  return (
    <div className="mt-2 space-y-3 text-sm" data-testid="change-detail">
      <p className="text-paper-dim">{c.explanation}</p>
      <SideTable c={c} />
      <p className="text-xs text-paper-mute" data-testid="change-method">
        Method: {c.methodChanged ? 'changed between these vintages' : 'unchanged'}
        {c.methodUnverified ? ' (fingerprint unverified: built from uncommitted code)' : ''}. Input data:{' '}
        {pair?.dataRelation === 'same' ? 'verifiably identical' : pair?.dataRelation === 'changed' || pair?.dataChanged ? 'changed' : 'not recorded, so a change cannot be ruled out'}.
        Attribution: {ATTRIBUTION_LABEL[c.attribution]}.
      </p>
      {c.sources.length > 0 && (
        <p className="text-xs text-paper-mute" data-testid="change-sources">
          Inputs behind the moved fields: {c.sources.join(', ')} (config/raw_inputs_manifest.yml).
        </p>
      )}
      {impacts.length > 0 && (
        <div data-testid="change-impacts">
          <p className="eyebrow">Relevant intelligence (reviewed)</p>
          <ul className="mt-1 list-disc pl-5 text-xs text-paper-dim">
            {impacts.map((i) => (
              <li key={i.impact_id}>
                {i.event_slug ?? i.event_id}: {i.reason} <span className="text-paper-mute">({i.dimension}; accepted by {i.reviewer})</span>
              </li>
            ))}
          </ul>
        </div>
      )}
      <p className="text-xs text-paper-mute">
        Vintages {vintageDate(b, c.prev)} → {vintageDate(b, c.cur)} · {c.id}
      </p>
    </div>
  );
}

/** The block itself, given a loaded bundle (exported for tests). */
export function ChangeSinceBlock({ b, iso3, tech, stage }: { b: ChangesBundle; iso3: string; tech: string; stage: string }) {
  const recs = changesForCell(b, iso3, tech, stage);
  const last = latestPair(b);
  const win = detailWindow(b);
  if (!recs.length) {
    return win ? (
      <p className="mt-3 text-xs text-paper-mute" data-testid="change-since-none">
        No material change to this assessment between the {win.from} and {win.to} vintages.{' '}
        <Link className="link" to="/changes">
          What changed elsewhere &rarr;
        </Link>
      </p>
    ) : null;
  }
  const c = recs[0];
  const sinceLast = last !== null && c.prev === last.prev && c.cur === last.cur;
  return (
    <section className="mt-3 rounded-lg border border-white/10 bg-white/[0.03] p-3" aria-label="What changed" data-testid="change-since">
      <p className="eyebrow">{sinceLast ? 'Changed since last vintage' : `Last changed ${vintageDate(b, c.cur)}`}</p>
      <Lines c={c} />
      <p className="mt-2 text-xs text-paper-mute" data-testid="change-attribution">
        {ATTRIBUTION_LABEL[c.attribution]}
      </p>
      <details className="mt-2">
        <summary className="link cursor-pointer text-sm" data-testid="change-why">
          Why did this change?
        </summary>
        <ChangeDetail c={c} b={b} />
        {recs.length > 1 && <p className="mt-2 text-xs text-paper-mute">{recs.length - 1} earlier change(s) to this assessment are listed on the changes page.</p>}
      </details>
      <p className="mt-2 text-xs">
        <Link className="link" to={`/changes?country=${iso3}`}>
          All changes for this economy &rarr;
        </Link>
        <span className="sr-only">
          {' '}
          ({tech} {stageDisplay(stage)})
        </span>
      </p>
    </section>
  );
}

export function ChangeSince({ iso3, tech, stage }: { iso3: string; tech: string; stage: string }) {
  const { status, data } = useChanges();
  if (status !== 'ready' || !data) return null;
  return <ChangeSinceBlock b={data} iso3={iso3} tech={tech} stage={stage} />;
}
