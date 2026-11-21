// /chart-of-the-day/:date? — one chart a day from raw data (scripts/chart_of_day/01_chart_of_day.R), drawn with a
// weighting that favours the countries, technologies and steps the AIB analysis cares about most. Shows the chosen
// day (default: the latest up to today), previous/next, a link into the matching Explorer page, and the archive.
// Days dated after today are drafts and stay hidden.

import { useEffect, useMemo, useState } from 'react';
import { Link, useParams } from 'react-router';
import { useTitle } from '../shell/title.ts';
import { BlocLegend, ChartOfDayFigure, SeriesLegend, ShiftLegend, TradeLegend } from './ChartOfDayFigure.tsx';
import { loadDay, loadIndex, longDate, pickDay, publishedDays, todayIso, type CodIndexDay, type CodSpec } from './data.ts';

type State = { status: 'loading' } | { status: 'error'; message: string } | { status: 'ready'; days: CodIndexDay[] };

export function useCodIndex(): State {
  const [s, setS] = useState<State>({ status: 'loading' });
  useEffect(() => {
    let live = true;
    loadIndex()
      .then((days) => live && setS({ status: 'ready', days }))
      .catch((e: unknown) => live && setS({ status: 'error', message: String(e) }));
    return () => {
      live = false;
    };
  }, []);
  return s;
}

export function useCodDay(day: CodIndexDay | null): CodSpec | null | 'error' {
  const [spec, setSpec] = useState<CodSpec | null | 'error'>(null);
  useEffect(() => {
    if (!day) return;
    let live = true;
    setSpec(null);
    loadDay(day)
      .then((s) => live && setSpec(s))
      .catch(() => live && setSpec('error'));
    return () => {
      live = false;
    };
  }, [day]);
  return spec;
}

export function ChartCardBody({ spec, compact = false }: { spec: CodSpec; compact?: boolean }) {
  return (
    <>
      <h2 className={compact ? 'text-lg font-medium leading-snug text-paper' : 'text-xl font-medium leading-snug text-paper sm:text-2xl'} data-testid="cod-title">
        {spec.title}
      </h2>
      <p className="mt-2 text-sm text-paper-dim" data-testid="cod-subtitle">
        {spec.subtitle}
      </p>
      <div className="mt-4 overflow-x-auto">
        <TradeLegend spec={spec} />
        <SeriesLegend spec={spec} />
        <ChartOfDayFigure spec={spec} compact={compact} />
      </div>
      <BlocLegend spec={spec} />
      <ShiftLegend spec={spec} />
      {!compact && (
        <div className="mt-4 space-y-1 text-xs text-paper-mute">
          {spec.notes && <p>{spec.notes}</p>}
          <p>Source: {spec.source}.</p>
        </div>
      )}
    </>
  );
}

export default function ChartOfDayView() {
  const { date } = useParams();
  const idx = useCodIndex();
  const today = todayIso();
  const pub = useMemo(() => (idx.status === 'ready' ? publishedDays(idx.days, today) : []), [idx, today]);
  const day = useMemo(() => (idx.status === 'ready' ? pickDay(idx.days, today, date) : null), [idx, today, date]);
  const spec = useCodDay(day);
  useTitle('Chart of the day');

  const i = day ? pub.findIndex((d) => d.date === day.date) : -1;
  const newer = i > 0 ? pub[i - 1] : null;
  const older = i >= 0 && i < pub.length - 1 ? pub[i + 1] : null;

  return (
    <div className="mx-auto w-full max-w-5xl px-4 py-10 sm:px-6 lg:px-8" data-testid="cod-view">
      <div className="eyebrow mb-2">Chart of the day</div>
      <div className="rule-neis mb-4" aria-hidden="true" />
      {idx.status === 'loading' && <p className="text-paper-dim">Loading…</p>}
      {idx.status === 'error' && <p className="text-paper-dim">No charts yet. Run scripts/chart_of_day/01_chart_of_day.R to generate them.</p>}
      {idx.status === 'ready' && !day && (
        <p className="text-paper-dim" data-testid="cod-missing">
          {date ? `There is no chart for ${date}.` : 'No chart has been published yet.'}{' '}
          {pub[0] && (
            <Link className="link" to={`/chart-of-the-day/${pub[0].date}`}>
              See the latest
            </Link>
          )}
        </p>
      )}
      {day && (
        <>
          <div className="mb-4 flex flex-wrap items-baseline justify-between gap-3">
            <p className="text-sm text-paper-mute" data-testid="cod-date">
              {longDate(day.date)}
            </p>
            <nav aria-label="Other days" className="flex gap-4 text-sm">
              {older && (
                <Link className="link" to={`/chart-of-the-day/${older.date}`} data-testid="cod-older">
                  &larr; Previous day
                </Link>
              )}
              {newer && (
                <Link className="link" to={`/chart-of-the-day/${newer.date}`} data-testid="cod-newer">
                  Next day &rarr;
                </Link>
              )}
            </nav>
          </div>
          <article className="panel p-6" style={{ borderTop: '4px solid #f58229' }}>
            {spec === null && <p className="text-paper-dim">Loading…</p>}
            {spec === 'error' && <p className="text-paper-dim">This chart could not be loaded.</p>}
            {spec && spec !== 'error' && (
              <>
                <ChartCardBody spec={spec} />
                <div className="mt-5 flex flex-wrap gap-3">
                  <Link to={spec.explore} className="btn btn-primary" data-testid="cod-explore">
                    Explore this in the {spec.explore.startsWith('/countries') ? 'Country' : 'Sector'} pages
                  </Link>
                  <Link to={`/countries/${spec.focus.iso3}`} className="btn">
                    {spec.focus.name}: country strategy
                  </Link>
                </div>
              </>
            )}
          </article>
          <p className="mt-3 text-xs text-paper-mute">
            A new chart every day, chosen at random from raw data (trade, investment, minerals and costs), with more weight given to the countries,
            technologies and supply-chain steps the allied industrial base analysis focuses on.
          </p>
        </>
      )}
      {pub.length > 1 && (
        <section className="mt-10" aria-labelledby="cod-archive">
          <h2 id="cod-archive" className="eyebrow mb-3 text-paper">
            Earlier charts
          </h2>
          <ul className="divide-y divide-white/10 text-sm" data-testid="cod-archive-list">
            {pub.map((d) => (
              <li key={d.date} className="flex gap-4 py-2">
                <span className="w-24 shrink-0 tabular text-paper-mute">{d.date}</span>
                {d.date === day?.date ? (
                  <span className="text-paper">{d.title}</span>
                ) : (
                  <Link className="link" to={`/chart-of-the-day/${d.date}`}>
                    {d.title}
                  </Link>
                )}
              </li>
            ))}
          </ul>
        </section>
      )}
    </div>
  );
}
