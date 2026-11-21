// /export/chart-of-the-day/:date — the day's chart as a self-contained card for PNG export (no header, no nav):
// the dashboard's dark ground and type, the NEIS logo, title, subtitle, chart, key, notes, source and the date.
// 1200 px wide; the height follows the chart. Unlike the public page it shows any generated date, so drafts can be
// exported ahead of their day. dashboard/scripts/chart-of-day-png.mjs screenshots [data-cod-ready] with headless Edge.

import { useEffect, useMemo, useRef, useState } from 'react';
import { useParams } from 'react-router';
import { Logo } from '../components/Logo.tsx';
import { BlocLegend, ChartOfDayFigure, SeriesLegend, ShiftLegend, TradeLegend } from './ChartOfDayFigure.tsx';
import { useCodDay, useCodIndex } from './ChartOfDayView.tsx';
import { longDate } from './data.ts';

export const EXPORT_WIDTH = 1200;
const TEXT_SCALE = 1.35;

export default function ExportCard() {
  const { date } = useParams();
  const idx = useCodIndex();
  const day = useMemo(() => (idx.status === 'ready' ? (idx.days.find((d) => d.date === date) ?? null) : null), [idx, date]);
  const spec = useCodDay(day);
  const ref = useRef<HTMLDivElement>(null);
  const [ready, setReady] = useState(false);

  // ready once the chart's SVG is in and fonts have loaded (Plot draws after a width measurement)
  useEffect(() => {
    if (!spec || spec === 'error') return;
    let live = true;
    const tick = async () => {
      await document.fonts.ready;
      for (let i = 0; i < 100 && live; i++) {
        if (ref.current?.querySelector('.plot-host svg')) break;
        await new Promise((r) => setTimeout(r, 50));
      }
      await new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r)));
      if (live) setReady(true);
    };
    void tick();
    return () => {
      live = false;
    };
  }, [spec]);

  useEffect(() => {
    document.documentElement.style.background = '#111827';
    document.body.style.margin = '0';
  }, []);

  if (idx.status === 'error' || spec === 'error') return <div data-cod-error="1">Chart not found.</div>;
  if (idx.status === 'ready' && !day) return <div data-cod-error="1">No chart for {date}.</div>;
  if (!spec) return null;

  return (
    <div
      ref={ref}
      data-cod-ready={ready ? '1' : undefined}
      className="bg-ink-900 text-paper"
      style={{ width: EXPORT_WIDTH, boxSizing: 'border-box', padding: '0 56px 40px', borderTop: '8px solid #f58229' }}
    >
      <header className="flex items-center justify-between" style={{ paddingTop: 32, paddingBottom: 24 }}>
        <Logo className="h-12 w-auto text-paper" />
        <div className="text-right">
          <div className="eyebrow" style={{ fontSize: 15, letterSpacing: '0.12em' }}>
            Chart of the day
          </div>
          <div className="text-paper-mute" style={{ fontSize: 15, marginTop: 4 }}>
            {longDate(spec.date)}
          </div>
        </div>
      </header>
      <div className="rule-neis" aria-hidden="true" style={{ marginBottom: 28 }} />
      <h1 className="font-medium text-paper" style={{ fontSize: 38, lineHeight: 1.18, letterSpacing: '-0.01em', margin: 0 }}>
        {spec.title}
      </h1>
      <p className="text-paper-dim" style={{ fontSize: 20, lineHeight: 1.4, marginTop: 14, marginBottom: 0 }}>
        {spec.subtitle}
      </p>
      <div style={{ marginTop: 28 }}>
        <TradeLegend spec={spec} className="text-base" />
        <SeriesLegend spec={spec} className="text-base" />
        <ChartOfDayFigure spec={spec} textScale={TEXT_SCALE} />
        <BlocLegend spec={spec} className="text-base" />
        <ShiftLegend spec={spec} className="text-base" />
      </div>
      <footer className="text-paper-mute" style={{ marginTop: 28, paddingTop: 16, borderTop: '1px solid rgb(255 255 255 / 0.15)', fontSize: 14, lineHeight: 1.5 }}>
        {spec.notes && <p style={{ margin: '0 0 6px' }}>{spec.notes}</p>}
        <p style={{ margin: 0 }}>
          Source: {spec.source}. Chart: NEIS Center, Allied Industrial Base Explorer.
        </p>
      </footer>
    </div>
  );
}
