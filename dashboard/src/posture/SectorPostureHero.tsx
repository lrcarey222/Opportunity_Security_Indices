// The Sector view's headline: pick a technology and a six-stage node, see every country on the need x
// opportunity plane with the takeaway sentence and counts. Click a point for that country's profile.
// The node lives in the URL (?pnode=Tech|Stage) so a link opens the same picture.

import { useMemo } from 'react';
import { useNavigate, useSearchParams } from 'react-router';
import { Loading, Notice, Select } from '../components/ui.tsx';
import type { PostureCell } from './contract.ts';
import { countPostures, sectorTakeaway } from './derive.ts';
import { MODE_LABEL, POSTURE_COLOR, POSTURE_LABEL } from './grammar.ts';
import { PostureLegend, Swatch, TradeFreshness } from './PostureLegend.tsx';
import { PostureScatter } from './PostureScatter.tsx';
import { usePostureCore } from './store.ts';

export const SECTOR_NODE_KEY = 'pnode';
const DEFAULT_NODE = 'Batteries|Refining';

export function SectorPostureHero() {
  const core = usePostureCore();
  const [sp, setSp] = useSearchParams();
  const nav = useNavigate();
  const raw = sp.get(SECTOR_NODE_KEY) ?? DEFAULT_NODE;
  const [tech, stage] = raw.split('|');
  const cells = useMemo<PostureCell[]>(() => (core.data ? core.data.cells.filter((c) => c.tech === tech && c.stage === stage) : []), [core.data, tech, stage]);
  const names = useMemo(() => Object.fromEntries((core.data?.countries ?? []).map((c) => [c.iso3, c.name])), [core.data]);
  if (core.status === 'error') {
    return (
      <Notice kind="warning" title="The posture data could not be loaded">
        {core.error}
      </Notice>
    );
  }
  if (!core.data) return <Loading label="Loading the strategic posture" />;
  const set = (t: string, s: string) => {
    const q = new URLSearchParams(sp);
    q.set(SECTOR_NODE_KEY, `${t}|${s}`);
    setSp(q, { replace: true, preventScrollReset: true });
  };
  const techs = core.data.meta.techs;
  const stages = core.data.meta.stages.filter((s) => s !== 'Deployment');
  const okTech = techs.includes(tech) ? tech : techs[0];
  const okStage = stages.includes(stage) ? stage : stages[0];
  const n = countPostures(cells);
  return (
    <section className="card p-5" aria-labelledby="sector-posture-title" data-testid="sector-posture-hero">
      <div className="flex flex-wrap items-end justify-between gap-x-6 gap-y-3">
        <div className="min-w-0 max-w-3xl">
          <h2 id="sector-posture-title" className="rule-neis-sm text-base font-medium text-paper lg:text-lg">
            Who should secure, compete or deploy in this node
          </h2>
          <p className="mt-2 text-sm text-paper-dim" data-testid="posture-takeaway">
            {sectorTakeaway(okTech, okStage, cells, names)}
          </p>
        </div>
        <div className="flex flex-wrap gap-3">
          <Select label="Technology" value={okTech} options={techs.map((t) => ({ value: t, label: t }))} onChange={(t) => set(t, okStage)} />
          <Select label="Stage" value={okStage} options={stages.map((s) => ({ value: s, label: s }))} onChange={(s) => set(okTech, s)} className="min-w-[9rem]" />
        </div>
      </div>
      <dl className="mt-3 flex flex-wrap gap-2" aria-label="Posture counts">
        {(['S', 'C', 'D'] as const).map((p) => (
          <div key={p} className="flex items-center gap-2 rounded-md bg-ink-900/60 px-3 py-1.5">
            <Swatch color={POSTURE_COLOR[p]} size={12} />
            <dt className="text-xs text-paper-mute">{POSTURE_LABEL[p]}</dt>
            <dd className="tabular text-sm font-medium text-paper">{n[p]}</dd>
          </div>
        ))}
        <div className="flex items-center gap-2 rounded-md bg-ink-900/60 px-3 py-1.5 text-xs text-paper-mute">
          {(['catch_up', 'preserve', 'partner'] as const).map((m) => (
            <span key={m}>
              {MODE_LABEL[m]} <span className="tabular text-paper">{n[m]}</span>
            </span>
          ))}
        </div>
      </dl>
      <div className="mt-2">
        <TradeFreshness meta={core.data.meta} />
      </div>
      <div className="mt-3">
        <PostureScatter core={core.data} cells={cells} onOpen={(iso) => nav(`/countries/${iso}`)} />
      </div>
      <div className="mt-3 space-y-1">
        <PostureLegend alignment={false} />
        <p className="text-xs text-paper-mute">
          Outlined diamonds are Secure: they keep their true coordinates because Secure overrides the quadrant. Point size is Competitive Position. Countries without a measured need or
          opportunity are not plotted ({cells.length - cells.filter((c) => c.need !== null && c.opp !== null).length} here).
        </p>
      </div>
    </section>
  );
}
