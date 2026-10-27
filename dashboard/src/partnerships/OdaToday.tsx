// The Development lens's Today panel (2026-10-19), loaded lazily (React.lazy) so the Sector bundle's code and
// its sector-meta and sector-dev chunks reach a page only when its Development tab is open: official
// development aid (OECD CRS), donor -> recipient, summed over the aid sectors in scope and the years. For an
// economy, its donors and its recipients; without one, the largest flows. It reports the partners it names
// to the page (onNames) for the cross-lens summary. Export credits are not in this build: the panel says so.

import { useEffect, useMemo } from 'react';
import { Loading } from '../components/ui.tsx';
import { useSectorChunk, useSectorManifest, useSectorMeta, useSectorStore } from '../sector/data/SectorDataProvider.tsx';
import type { SectorModel } from '../sector/data/model.ts';
import { fmtUsdM } from '../views/Partners/ties.ts';
import { lensNames, lensNone, LENS_LOADING, type LensNames } from './lenses.ts';
import { odaPartners, odaRecipients, odaView } from './oda.ts';
import { LensCard, RankedNames } from './PartnershipLenses.tsx';

export interface OdaTodayProps {
  /** the economy (ISO3), or null for every economy */
  iso: string | null;
  /** the economy in prose ("Japan") */
  focusName?: string;
  /** a supply-chain stage ("Midstream") limiting the aid sectors, or null for all of them */
  stage: string | null;
  /** a link to an economy's page (null: plain text) */
  href?: (iso3: string) => string | null;
  /** the partners the panel names (the cross-lens summary) */
  onNames: (n: LensNames) => void;
  /** the cards' heading level */
  level?: 3 | 4;
}

/** The aid-sector items in scope: those linked to the stage's cells, or every one. */
function aidItems(model: SectorModel, stage: string | null) {
  const all = model.items.filter((i) => i.axis === 'aid_sector');
  if (!stage) return all;
  const ids = new Set(model.cells.filter((c) => c.stage === stage).flatMap((c) => model.itemsOfCell(c.idx, 'aid_sector').map((x) => x.item.idx)));
  return all.filter((i) => ids.has(i.idx));
}

const EXPORT_CREDIT = 'Export credits (official export finance) are not in this build yet, so the aid below is only part of development finance.';

export default function OdaToday({ iso, focusName, stage, href, onNames, level = 3 }: OdaTodayProps) {
  const store = useSectorStore();
  const man = useSectorManifest();
  const meta = useSectorMeta();
  const dev = useSectorChunk('sector-dev', true);
  const model = meta.model;
  const has = man.status !== 'ready' || store.hasChunk('sector-dev');
  const items = useMemo(() => (model ? aidItems(model, stage) : []), [model, stage]);
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
  if (!view || !model) return <Loading label="Loading the aid flows" />;
  const nameOf = (g: number) => model.geos[g]?.name ?? code(g);
  const what = items.length === 1 ? `aid for ${items[0].label.toLowerCase()}` : stage ? `aid for the aid sectors of ${stage}` : 'aid for energy, industry and mining';
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
        <p className="text-xs text-paper-mute">Source: OECD Creditor Reporting System, gross disbursements. {EXPORT_CREDIT}</p>
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
      <p className="text-xs text-paper-mute">Source: OECD Creditor Reporting System, gross disbursements. {EXPORT_CREDIT}</p>
    </div>
  );
}
