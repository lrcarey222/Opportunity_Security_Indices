// SupplierMatrix (spec_country.md A4.2 S5, "Who supplies {Country} in each sector"): rows = up to 12
// economies (the top-5 suppliers of each high-priority sector and the top-10 average suppliers), columns =
// the country's sectors with imports in the trade year, grouped by the six supply-chain stages in chain order
// with text headers (a one-column group shows the stage's short form, the full name for screen readers and
// on hover; stages without a traded column are left out: Deployment has no trade basket). Dot area is
// proportional to the import share (r = 2 + 10 sqrt(share)), neutral fill; the selected partner's row has
// white dots and an orange outline; cells among the selected partner's Friendshore best five carry a star
// and the words "best five". An HTML table (it reads as one), scrolling inside the card below 1024 px.

import { STAGE_LABEL, STAGE_SHORT, SUPPLY_STAGES } from '../../lib/stages.ts';
import type { FactSheet } from '../contract.ts';
import { GROUP_WORD, supplierModel } from '../derive/model.ts';
import { proseName, poss } from '../derive/text.ts';

function fmtShare(x: number): string {
  if (x > 0 && x < 0.0005) return '<0.1%';
  return `${(100 * x).toFixed(1)}%`;
}

function fmtUsd(v: number | null): string {
  if (v === null || !Number.isFinite(v)) return '';
  const a = Math.abs(v);
  const steps: [number, string][] = [[1e12, 'tn'], [1e9, 'bn'], [1e6, 'm'], [1e3, 'k']];
  for (const [s, w] of steps) if (a >= s) return `USD ${Number((v / s).toPrecision(3))} ${w}`;
  return `USD ${Number(v.toPrecision(3))}`;
}

export function SupplierMatrix({ sheet, selected, onSelect }: { sheet: FactSheet; selected: string | null; onSelect: (iso3: string) => void }) {
  const m = supplierModel(sheet);
  const name = proseName(sheet.meta.country);
  if (!m.cells.length || !m.partners.length) return <p className="text-sm text-paper-mute">No imports recorded in {sheet.meta.trade_year} for {poss(name)} sectors with a trade basket.</p>;
  const order = SUPPLY_STAGES.flatMap((st) => m.cells.map((c, j) => ({ c, j })).filter((x) => x.c.stage === st));
  const groups = SUPPLY_STAGES.map((st) => ({ st, n: order.filter((x) => x.c.stage === st).length })).filter((g) => g.n > 0);
  const sel = sheet.partners.rows.find((r) => r.iso3 === selected) ?? null;
  const best = new Set(sel?.fs_best5 ?? []);
  const year = sheet.meta.trade_year;
  return (
    <div className="relative overflow-x-auto" tabIndex={0} role="region" aria-label={`Who supplies ${name} in each sector: a table that scrolls sideways`}>
      <table className="border-collapse text-xs">
        <caption className="sr-only">Share of {poss(name)} imports in each sector supplied by each economy, {year}</caption>
        <thead>
          <tr>
            <th scope="col" className="sticky left-0 z-10 bg-ink-800" />
            {groups.map((g) => (
              <th key={g.st} scope="colgroup" colSpan={g.n} className="border-b hairline px-1 pb-1 text-left" title={STAGE_LABEL[g.st]}>
                {g.n > 1 ? (
                  <span className="eyebrow">{STAGE_LABEL[g.st]}</span>
                ) : (
                  <>
                    <span className="eyebrow" aria-hidden="true">
                      {STAGE_SHORT[g.st]}
                    </span>
                    <span className="sr-only">{STAGE_LABEL[g.st]}</span>
                  </>
                )}
              </th>
            ))}
          </tr>
          <tr>
            <th scope="col" className="sticky left-0 z-10 bg-ink-800 pr-3 text-left align-bottom">
              <span className="eyebrow">Supplier</span>
            </th>
            {order.map(({ c }) => (
              <th key={c.key} scope="col" className="w-14 min-w-14 px-1 pb-1 text-center align-bottom font-normal text-paper-mute" title={c.label}>
                <span className="block leading-tight">{c.tech}</span>
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {m.partners.map((p, i) => {
            const on = p.iso3 === selected;
            return (
              <tr key={p.iso3} className={on ? 'outline outline-2 -outline-offset-2 outline-signal' : undefined}>
                <th scope="row" className="sticky left-0 z-10 bg-ink-800 py-1 pr-3 text-left font-normal">
                  <button type="button" className={`text-left ${on ? 'text-paper' : 'text-paper-dim hover:text-paper'}`} aria-pressed={on} onClick={() => onSelect(p.iso3)}>
                    {p.name} <span className="text-paper-mute">({GROUP_WORD[p.group]})</span>
                  </button>
                </th>
                {order.map(({ c, j }) => {
                  const s = m.share[i]?.[j] ?? null;
                  const v = m.value[i]?.[j] ?? null;
                  const st = m.status[i]?.[j] ?? null;
                  const star = on && best.has(c.key);
                  const tip = s === null
                    ? `${p.name}: no imports into ${poss(name)} ${c.label_prose} in ${year}`
                    : `${p.name} supplied ${fmtShare(s)} of ${poss(name)} ${c.label_prose} imports in ${year}${v !== null ? ` (${fmtUsd(v)})` : ''}${st === 'mirror' ? '; read from the partner’s report' : ''}${star ? '; among its Friendshore best five' : ''}`;
                  const r = s === null ? 0 : 2 + 10 * Math.sqrt(s);
                  return (
                    <td key={c.key} className="h-9 w-14 p-0 text-center align-middle" title={tip}>
                      <span className="sr-only">{tip}</span>
                      {s !== null && (
                        <svg width="28" height="28" viewBox="-14 -14 28 28" aria-hidden="true" className="inline-block align-middle">
                          <circle r={Math.min(12, r)} fill={on ? '#ffffff' : '#d1d5db'} />
                        </svg>
                      )}
                      {star && (
                        <span aria-hidden="true" className="ml-0.5 align-top text-[10px] text-signal">
                          ★
                        </span>
                      )}
                    </td>
                  );
                })}
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

