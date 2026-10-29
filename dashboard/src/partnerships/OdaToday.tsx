// The Development lens's Today panel (2026-10-19), loaded lazily (React.lazy) so the Sector bundle's code and
// its sector-meta and sector-dev chunks reach a page only when its Development tab is open: official
// development aid (OECD CRS), donor -> recipient, summed over the aid sectors in scope and the years. For an
// economy, its donors and its recipients; without one, the largest flows. It reports the partners it names
// to the page (onNames) for the cross-lens summary. Export credits are not in this build: the panel says so.
// With a stage (six stages since 2026-10-29: the Sector bundle's cells, labels such as "Refining"), only the
// aid sectors linked to that stage's cells (oda.ts); a stage without any says so and names the stages that
// have aid, and a stage whose aid sector is shared with other stages says it is the same aid.

import { useEffect, useMemo } from 'react';
import { Loading } from '../components/ui.tsx';
import { useSectorChunk, useSectorManifest, useSectorMeta, useSectorStore } from '../sector/data/SectorDataProvider.tsx';
import type { SectorModel } from '../sector/data/model.ts';
import { fmtUsdM } from '../views/Partners/ties.ts';
import { lensNames, lensNone, LENS_LOADING, type LensNames } from './lenses.ts';
import { stageLabel } from '../lib/stages.ts';
import { aidItemsOfStage, aidSharedStages, aidStages, odaPartners, odaRecipients, odaView, type AidCellLink } from './oda.ts';
import { LensCard, RankedNames } from './PartnershipLenses.tsx';

export interface OdaTodayProps {
  /** the economy (ISO3), or null for every economy */
  iso: string | null;
  /** the economy in prose ("Japan") */
  focusName?: string;
  /** a supply-chain stage ("Refining") limiting the aid sectors, or null for all of them */
  stage: string | null;
  /** a link to an economy's page (null: plain text) */
  href?: (iso3: string) => string | null;
  /** the partners the panel names (the cross-lens summary) */
  onNames: (n: LensNames) => void;
  /** the cards' heading level */
  level?: 3 | 4;
}

/** Each Sector cell's stage and its aid-sector items. */
function aidLinks(model: SectorModel): AidCellLink[] {
  return model.cells.map((c) => ({ stage: c.stage, items: model.itemsOfCell(c.idx, 'aid_sector').map((x) => x.item.idx) }));
}

/** 'A', 'A and B', 'A, B and C'. */
const andWords = (xs: readonly string[]): string => (xs.length <= 1 ? (xs[0] ?? '') : `${xs.slice(0, -1).join(', ')} and ${xs[xs.length - 1]}`);

const EXPORT_CREDIT = 'Export credits (official export finance) are not in this build yet, so the aid below is only part of development finance.';

export default function OdaToday({ iso, focusName, stage, href, onNames, level = 3 }: OdaTodayProps) {
  const store = useSectorStore();
  const man = useSectorManifest();
  const meta = useSectorMeta();
  const dev = useSectorChunk('sector-dev', true);
  const model = meta.model;
  const has = man.status !== 'ready' || store.hasChunk('sector-dev');
  const links = useMemo(() => (model ? aidLinks(model) : []), [model]);
  const items = useMemo(() => {
    if (!model) return [];
    const all = model.items.filter((i) => i.axis === 'aid_sector');
    const ids = new Set(aidItemsOfStage(links, all.map((i) => i.idx), stage));
    return all.filter((i) => ids.has(i.idx));
  }, [model, links, stage]);
  // a stage without any aid sector (Recovery): the stages that have aid, for the notice
  const noAid = !!model && !!stage && items.length === 0;
  const geo = model && iso ? (model.geoByCode.get(iso)?.idx ?? null) : null;
  const view = useMemo(() => (dev.data && model ? odaView(dev.data, items.map((i) => i.idx), geo) : null), [dev.data, model, items, geo]);
  const isCountry = (g: number) => {
    const x = model?.geos[g];
    return !!x && (x.kind === 'country' || x.kind === 'eu');
  };
  const code = (g: number) => model?.geos[g]?.code ?? String(g);
  const failed = man.status === 'error' || meta.status === 'error' || dev.status === 'error';
  const names: LensNames = !has
    ? lensNone('this data build has no aid flows')
    : failed
      ? lensNone('the aid flows could not be loaded')
      : noAid
        ? lensNone(`no aid sector is linked to ${stageLabel(stage as string)}`)
        : !view
        ? LENS_LOADING
        : iso && geo === null
          ? lensNone(`no aid data for ${focusName ?? iso}`)
          : lensNames((iso ? odaPartners(view) : odaRecipients(view)).filter(isCountry).map(code), 10, iso);
  const sig = `${names.status}|${names.keys.join(',')}|${names.reason ?? ''}`;
  useEffect(() => {
    onNames(names);
    // the signature carries the value
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [sig, onNames]);
  if (!has) return <p className="text-sm text-paper-mute" data-testid="lens-empty">This data build has no aid flows. {EXPORT_CREDIT}</p>;
  if (failed) {
    return (
      <p className="text-sm text-paper-mute" data-testid="lens-empty">
        The aid flows could not be loaded.{' '}
        <button type="button" className="link" onClick={() => (dev.status === 'error' ? dev.retry() : man.retry())}>
          Retry
        </button>
      </p>
    );
  }
  if (noAid) {
    const withAid = aidStages(links).map(stageLabel);
    return (
      <p className="text-sm text-paper-mute" data-testid="lens-empty">
        No aid sector is linked to {stageLabel(stage as string)}
        {withAid.length ? `: the aid data cover ${andWords(withAid)}` : ''}. {EXPORT_CREDIT}
      </p>
    );
  }
  if (!view || !model) return <Loading label="Loading the aid flows" />;
  const nameOf = (g: number) => model.geos[g]?.name ?? code(g);
  const what = items.length === 1 ? `aid for ${items[0].label.toLowerCase()}` : stage ? `aid for the aid sectors of ${stageLabel(stage)}` : 'aid for energy, industry and mining';
  // the same aid on other stages (industry: Refining, Components and Assembly), said once
  const shared = stage ? aidSharedStages(links, items.map((i) => i.idx), stage).map(stageLabel) : [];
  const sharedNote = shared.length ? ` The aid data do not split this by stage: the same ${what} is shown at ${andWords(shared)}.` : '';
  const span = view.years ? `${view.years[0]}-${view.years[1]}` : '';
  const unit = 'USD, constant prices, summed over the years';
  const link = (g: number) => (isCountry(g) ? (href?.(code(g)) ?? null) : null);
  if (iso && geo === null) return <p className="text-sm text-paper-mute" data-testid="lens-empty">No aid data for {focusName ?? iso} in this build. {EXPORT_CREDIT}</p>;
  if (iso) {
    const who = focusName ?? nameOf(geo as number);
    return (
      <div className="space-y-4">
        <div className="grid gap-4 lg:grid-cols-2">
          <LensCard id={`oda-in-${iso}`} level={level} title={`Aid received by ${who}`} sub={`Official development ${what}, by donor, ${span} (${unit}).`} testId="oda-received">
            {view.received.length ? (
              <RankedNames rows={view.received} max={8} label={`Donors of ${who}`} keyOf={(r) => String(r.geo)} nameOf={(r) => nameOf(r.geo)} href={(r) => link(r.geo)} value={(r) => fmtUsdM(r.v)} />
            ) : (
              <p className="text-paper-mute">No donor in the data recorded {what} to {who}.</p>
            )}
          </LensCard>
          <LensCard id={`oda-out-${iso}`} level={level} title={`Aid given by ${who}`} sub={`What ${who} disbursed as official development ${what}, by recipient, ${span}.`} testId="oda-given">
            {view.given.length ? (
              <RankedNames rows={view.given} max={8} label={`Recipients of aid from ${who}`} keyOf={(r) => String(r.geo)} nameOf={(r) => nameOf(r.geo)} href={(r) => link(r.geo)} value={(r) => fmtUsdM(r.v)} />
            ) : (
              <p className="text-paper-mute">{who} is not recorded as a donor of {what}.</p>
            )}
          </LensCard>
        </div>
        <p className="text-xs text-paper-mute">
          Source: OECD Creditor Reporting System, gross disbursements.{sharedNote} {EXPORT_CREDIT}
        </p>
      </div>
    );
  }
  return (
    <div className="space-y-4">
      <LensCard id="oda-flows" level={level} title="The largest aid flows" sub={`Official development ${what}, donor to recipient, ${span} (${unit}).`} testId="oda-flows">
        {view.flows.length ? (
          <RankedNames
            rows={view.flows}
            max={10}
            label="The largest aid flows, donor to recipient"
            keyOf={(r) => `${r.from}|${r.to}`}
            nameOf={(r) => `${nameOf(r.from)} → ${nameOf(r.to)}`}
            value={(r) => fmtUsdM(r.v)}
          />
        ) : (
          <p className="text-paper-mute">No aid flow in the data for this selection.</p>
        )}
      </LensCard>
      <p className="text-xs text-paper-mute">
        Source: OECD Creditor Reporting System, gross disbursements.{sharedNote} {EXPORT_CREDIT}
      </p>
    </div>
  );
}
