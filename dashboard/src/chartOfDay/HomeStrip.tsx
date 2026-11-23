// Home's Chart of the Day strip: today's chart, compact, under the four ways in. Its own lazy chunk, so Home does
// not carry Observable Plot until the strip loads; renders nothing when there is no chart to show.

import { useMemo } from 'react';
import { Link } from 'react-router';
import { ChartCardBody, useCodDay, useCodIndex } from './ChartOfDayView.tsx';
import { pickDay, todayIso } from './data.ts';

export default function HomeStrip() {
  const idx = useCodIndex();
  const day = useMemo(() => (idx.status === 'ready' ? pickDay(idx.days, todayIso()) : null), [idx]);
  const spec = useCodDay(day);
  if (!day || !spec || spec === 'error') return null;
  return (
    <section aria-labelledby="home-cod" className="panel mt-10 p-6" data-testid="home-cod">
      <div className="mb-3 flex flex-wrap items-baseline justify-between gap-2">
        <h2 id="home-cod" className="eyebrow text-paper">
          Chart of the day
        </h2>
        <Link to={`/chart-of-the-day/${day.date}`} className="link text-sm">
          Notes, sources and earlier charts &rarr;
        </Link>
      </div>
      <ChartCardBody spec={spec} compact />
    </section>
  );
}
