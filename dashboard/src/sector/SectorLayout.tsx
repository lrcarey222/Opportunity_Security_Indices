// The Sector Insights layout (spec_sector.md 2.3, 4.0): SectorDataProvider -> SectorUrlProvider ->
// SectorBar -> Outlet, plus the caption resolver, the page-notes registry, the hatch defs and the
// "Ignored in the link" notice. It loads the manifest and sector-meta (never the engine); a missing
// manifest shows the module-level "No sector data" screen with the developer hint; views render once
// meta is ready (each view loads its own chunks).

import { useEffect, useMemo, useState } from 'react';
import { Outlet, useLocation, useNavigate, useParams } from 'react-router';
import { Loading, Notice } from '../components/ui.tsx';
import { setSectorBuild } from '../shell/buildInfo.ts';
import { useTitle } from '../shell/title.ts';
import { HEADLINE_TECH_SLUGS, isStageSlug, isTechSlug, STAGE_LABELS, STAGE_SLUGS, TECH_LABELS, TECH_SLUGS, type StageSlug, type TechSlug } from './contract.ts';
import { factsIndexOf } from './data/facts.ts';
import { SectorDataProvider, useSectorManifest, useSectorMeta, useSectorStore } from './data/SectorDataProvider.tsx';
import type { SectorModel } from './data/model.ts';
import { richness } from './derive/cells.ts';
import { countryOptions } from './derive/groups.ts';
import { fmtExportDate, fmtSize } from './format.ts';
import { makeCaptionResolver } from './views/caption.ts';
import { PAGE } from './views/common.tsx';
import { useLoadingStatus } from './views/data.ts';
import { CaptionProvider, PageNotesProvider, SectorBar, SvgDefs, type SectorBarPicker } from './views/kit.tsx';
import { SectorUrlProvider, useSectorUrl } from './views/state.tsx';

/** Notes always in "Data notes" (spec 9: the dials apply to Strategic Insights only). */
const ALWAYS_NOTES = ['The Perspective dials of the Country profile never change these pages: they show measured values with no weights and no scores.'];

/** The module title while no view is mounted (loading, no data, errors); views set their own. */
function ModuleTitle() {
  useTitle('', 'Sector Insights');
  return null;
}

function NoSectorData({ message }: { message: string }) {
  return (
    <div className={PAGE}>
      <ModuleTitle />
      <Notice kind="info" title="No sector data">
        <p>{message}</p>
        <p className="mt-2 text-paper-mute">
          Developers: run <code>Rscript scripts/87_export_sector_data.R</code> from the repository root (or <code>npm run dev:fixture</code> for the synthetic bundle). The Country and Allied Industrial Base pages are unaffected.
        </p>
      </Notice>
    </div>
  );
}

function UrlProblems() {
  const { problems } = useSectorUrl();
  const loc = useLocation();
  const [dismissed, setDismissed] = useState<string | null>(null);
  if (!problems.length || dismissed === loc.search) return null;
  return (
    <div className="mb-4">
      <Notice kind="warning" title="Ignored in the link" onDismiss={() => setDismissed(loc.search)}>
        {problems.join('; ')}
      </Notice>
    </div>
  );
}

function Bar({ model }: { model: SectorModel | null }) {
  const { state, set } = useSectorUrl();
  const params = useParams();
  const navigate = useNavigate();
  const loc = useLocation();
  const manifest = useSectorManifest();
  const loading = useLoadingStatus();
  const tech = params.tech && isTechSlug(params.tech) ? params.tech : null;
  const stage = params.stage && isStageSlug(params.stage) ? params.stage : null;
  const onSectorRoute = !!tech && !['explore', 'concentration', 'minerals', 'sources', 'country', 'data'].includes(params.tech ?? '');
  const keep = (extra = '') => {
    const q = new URLSearchParams();
    if (state.c) q.set('c', state.c);
    if (state.eu === 'bloc') q.set('eu', 'bloc');
    const s = q.toString();
    return `${s ? `?${s}` : ''}${extra}`;
  };
  const picker: SectorBarPicker | null =
    onSectorRoute && tech && model
      ? {
          tech,
          techOptions: TECH_SLUGS.map((t) => ({ value: t, label: TECH_LABELS[t], group: (HEADLINE_TECH_SLUGS as readonly string[]).includes(t) ? 'Headline technologies' : 'Non-headline' })),
          onTech: (t) => {
            const s = stage ?? null;
            const next = t as TechSlug;
            const hasStage = s && model.cell(next, s as StageSlug);
            const firstStage = STAGE_SLUGS.find((x) => model.cell(next, x));
            const path = s ? `/sector/${next}/${hasStage ? s : (firstStage ?? 'midstream')}${params.iso3 ? `/${params.iso3}` : ''}` : `/sector/${next}`;
            navigate(`${path}${keep()}`);
          },
          stage,
          stages: STAGE_SLUGS.map((s) => {
            const cell = model.cell(tech, s);
            return { slug: s, label: STAGE_LABELS[s], coverage: cell ? richness(model, cell) : null, disabled: !cell, title: cell ? undefined : `${TECH_LABELS[tech]} has no ${STAGE_LABELS[s]} cell` };
          }),
          onStage: (s: StageSlug) => navigate(`/sector/${tech}/${s}${params.iso3 ? `/${params.iso3}` : ''}${keep()}`),
        }
      : null;
  const options = useMemo(() => (model ? countryOptions(model, state.eu) : []), [model, state.eu]);
  const onCountryRoute = onSectorRoute && !!stage && !!params.iso3;
  const highlightValue = onCountryRoute ? (params.iso3 ?? '').toUpperCase() : (state.c ?? '');
  const status = loading ?? (manifest.data ? `Sector data: export ${fmtExportDate(manifest.data.created)}` : '');
  const euDisabled = loc.pathname.startsWith('/sector/sources') ? 'Nothing on this page is counted per economy.' : null;
  return (
    <SectorBar
      picker={picker}
      summary={picker ? `${TECH_LABELS[tech as TechSlug]}${stage ? ` · ${STAGE_LABELS[stage]}` : ''}` : null}
      highlight={{
        value: highlightValue || null,
        options,
        onChange: (iso: string | null) => {
          if (onCountryRoute && iso) navigate(`/sector/${tech}/${stage}/${iso}${keep()}`);
          else if (onCountryRoute && !iso) navigate(`/sector/${tech}/${stage}${keep()}`);
          else set({ c: iso || null });
        },
      }}
      eu={{
        checked: state.eu === 'bloc',
        onChange: (b) => set({ eu: b ? 'bloc' : 'members', c: !b && state.c === 'EUU' ? null : state.c }),
        disabled: !!euDisabled,
        reason: euDisabled ?? undefined,
      }}
      status={status}
      notes={[...(manifest.data?.notes ?? []), ...ALWAYS_NOTES]}
    />
  );
}

function Inner() {
  const store = useSectorStore();
  const manifest = useSectorManifest();
  const meta = useSectorMeta();
  const model = meta.model;
  useEffect(() => {
    if (manifest.data) setSectorBuild(manifest.data);
  }, [manifest.data]);
  const resolve = useMemo(() => {
    if (!model) return () => ({ sub: '' });
    return makeCaptionResolver(model, () => {
      const f = store.chunkState('sector-facts').data;
      return f ? factsIndexOf(f) : null;
    });
  }, [model, store]);

  if (manifest.status === 'error' && manifest.error) {
    if (manifest.error.code === 'NO_DATA') return <NoSectorData message={manifest.error.message} />;
    return (
      <div className={PAGE}>
        <ModuleTitle />
        <Notice kind="error" title="The sector data could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={manifest.retry}>Retry</button>}>
          {manifest.error.message}
        </Notice>
      </div>
    );
  }
  const metaBytes = meta.bytes ?? store.chunkBytes('sector-meta');
  return (
    <SectorUrlProvider model={model}>
      <PageNotesProvider>
        <CaptionProvider resolve={resolve}>
          <SvgDefs />
          <Bar model={model} />
          <div className={PAGE}>
            <UrlProblems />
            {model ? (
              <Outlet />
            ) : meta.status === 'error' ? (
              <>
              <ModuleTitle />
              <Notice kind="error" title="The sector summary could not be loaded" action={<button type="button" className="btn btn-ghost btn-sm" onClick={meta.retry}>Retry</button>}>
                {meta.error?.message}
              </Notice>
              </>
            ) : (
              <>
                <ModuleTitle />
                <Loading label={`Loading the sector summary${metaBytes ? `, ${fmtSize(metaBytes)}` : ''}`} />
              </>
            )}
          </div>
        </CaptionProvider>
      </PageNotesProvider>
    </SectorUrlProvider>
  );
}

export default function SectorLayout() {
  return (
    <SectorDataProvider>
      <Inner />
    </SectorDataProvider>
  );
}
