// Home: "Which partner relationships are becoming more complementary?" (change layer for partnerships). The pairs
// whose shared priority compacts grew most in the latest AIB vintage that changed any, with the attribution of that
// vintage pair. Renders nothing without partnership history.

import { Link } from 'react-router';
import { AIB_ATTRIBUTION_LABEL, aibVintageDate, latestAibChange, type AibChanges } from './aib.ts';

export const HOME_PARTNERSHIPS_SHOWN = 6;

export function HomePartnershipsView({ c }: { c: AibChanges }) {
  const p = latestAibChange(c);
  if (!p) return null;
  const more = p.changes.filter((r) => r.direction === 'more').slice(0, HOME_PARTNERSHIPS_SHOWN);
  return (
    <section aria-labelledby="home-partnerships" className="mt-12" data-testid="home-partnerships">
      <h2 id="home-partnerships" className="rule-neis-sm mb-3 text-xl font-medium text-paper">
        Which partner relationships are becoming more complementary?
      </h2>
      <p className="mb-3 max-w-3xl text-sm text-paper-dim">
        Between the {aibVintageDate(c, p.prev)} and {aibVintageDate(c, p.cur)} AIB results, {p.n_more} pairs of economies gained shared priority compacts and {p.n_less}{' '}
        lost them ({p.n_new} new relationship theses, {p.n_dropped} dropped). {AIB_ATTRIBUTION_LABEL[p.attribution]}
        {p.attribution === 'unattributed' ? ': the AIB method changed between these results, so these are not credited to new data.' : '.'}
      </p>
      {more.length > 0 && (
        <ul className="grid gap-2 sm:grid-cols-2 lg:grid-cols-3">
          {more.map((r) => (
            <li key={`${r.a}|${r.b}`} className="panel p-3 text-sm">
              <Link className="link" to={`/aib/partnerships?country=${r.a}&partner=${r.b}`}>
                {r.a} · {r.b}
              </Link>
              <p className="mt-1 text-paper-dim">{r.explanation}</p>
            </li>
          ))}
        </ul>
      )}
    </section>
  );
}
