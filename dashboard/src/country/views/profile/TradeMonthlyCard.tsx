// Monthly trade by sub-component (additive 2026-10-19), a card of "Sector position": for the selected sector,
// one line per sub-component of its HS6 basket, indexed to 100 in the first of the economy's latest 12
// reported months, with the sector's total as a neutral line. Core members only (the companion trade file,
// scripts/99 -> scripts/88), loaded when the card comes near the viewport. The sector follows the page's
// selection; when the file has no monthly trade for it, the card shows the sector with the most trade and
// says so. Imports | Exports is local state (not in the URL).

import { useMemo, useState } from 'react';
import { ChartCard } from '../../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../../components/TableView.tsx';
import { Loading, Segmented, Select } from '../../../components/ui.tsx';
import { LabelledLines } from '../../../sector/charts/LabelledLines.tsx';
import { useNear } from '../../../sector/views/data.ts';
import { useTradeMonthly } from '../../data/hooks.ts';
import { fmtUsd, monthLabel, monthTick, pickTradeCell, tradeLines, tradeRows, unreportedMonths, type TradeFlow } from '../../derive/tradeMonthly.ts';
import { cap, proseName } from '../../derive/text.ts';
import { csvWithSource, useProfile } from './common.tsx';

const FLOW_WORD: Record<TradeFlow, string> = { imports: 'imports', exports: 'exports' };

export function TradeMonthlyCard() {
  const { sheet, entry, manifest, cell, setCell } = useProfile();
  const [ref, near] = useNear<HTMLDivElement>();
  const t = useTradeMonthly(entry, near);
  const [flow, setFlow] = useState<TradeFlow>('imports');
  const name = proseName(sheet.meta.country);
  const doc = t.data;
  const picked = useMemo(() => (doc ? pickTradeCell(doc, cell) : null), [doc, cell]);
  const lines = useMemo(() => (picked ? tradeLines(picked.cell, flow) : null), [picked, flow]);

  if (entry.tier !== 'core') return null;
  // the notice only when the export carried monthly files at all (otherwise the inputs were absent: no card)
  if (!entry.trade_monthly && !manifest.countries.some((x) => x.trade_monthly)) return null;
  if (!entry.trade_monthly) {
    return (
      <div ref={ref} className="card p-4 text-sm text-paper-mute" data-testid="trade-monthly-none">
        Monthly trade by sub-component is shown for coalition core members that report monthly trade to UN Comtrade; {name} publishes no monthly data there.
      </div>
    );
  }
  if (!doc || !picked || !lines) {
    return (
      <div ref={ref} className="card p-4">
        {t.status === 'error' ? (
          <p className="text-sm text-paper-mute">
            The monthly trade file could not be loaded.{' '}
            <button type="button" className="link" onClick={t.retry}>
              Try again
            </button>
          </p>
        ) : (
          <Loading label="Loading monthly trade" />
        )}
      </div>
    );
  }

  const c = picked.cell;
  const first = monthLabel(doc.window.first);
  const last = monthLabel(doc.window.last);
  const missing = unreportedMonths(doc, flow);
  const idx = lines.lines.flatMap((l) => {
    const base = l.points.find((p) => Number.isFinite(p.y))?.y ?? 0;
    return base > 0 ? l.points.filter((p) => Number.isFinite(p.y)).map((p) => (100 * p.y) / base) : [];
  });
  const log = idx.length > 0 && Math.min(...idx) > 0 && Math.max(...idx) > 400;
  const rows = tradeRows(doc, c, flow);
  const cols: Col<(typeof rows)[number]>[] = [
    { id: 'month', header: 'Month', value: (r) => r.month, cell: (r) => monthLabel(r.month) },
    { id: 'sub', header: 'Sub-component', value: (r) => r.sub_component, minWidth: 200 },
    { id: 'v', header: `${cap(FLOW_WORD[flow])}`, value: (r) => r.value_usd, cell: (r) => (r.value_usd === null ? 'not reported' : fmtUsd(r.value_usd)), num: true },
    { id: 'i', header: `Index (${first} = 100)`, value: (r) => r.index_first_month_100, cell: (r) => (r.index_first_month_100 === null ? '–' : r.index_first_month_100.toFixed(1)), num: true },
  ];
  const notes: string[] = [];
  if (picked.fallback) {
    const sel = sheet.cells.find((x) => x.key === cell)?.label ?? 'The selected sector';
    notes.push(`${sel} has no traded-goods basket (HS6 codes); showing ${c.label}, the sector with the most monthly trade.`);
  }
  if (lines.notIndexed.length) notes.push(`Not indexed (no trade or no report in ${first}): ${lines.notIndexed.join(', ')}.`);
  if (lines.smaller.length) notes.push(`${lines.smaller.length} smaller sub-component${lines.smaller.length > 1 ? 's are' : ' is'} in the table: ${lines.smaller.join(', ')}.`);
  if (lines.none.length) notes.push(`No ${FLOW_WORD[flow]} in the window: ${lines.none.join(', ')}.`);
  if (missing.length) notes.push(`${cap(name)} has no complete report of ${FLOW_WORD[flow]} for ${missing.join(', ')} (not reported, or released for only part of the goods); the lines skip it.`);
  if (log) notes.push('Log scale: one sub-component grew more than fourfold from its first month.');
  const csv = () => ({
    name: `${sheet.meta.country} ${c.label} monthly ${FLOW_WORD[flow]}`,
    text: csvWithSource(
      rows.map((r) => ({ economy: sheet.meta.country, sector: c.label, flow: FLOW_WORD[flow], ...r })),
      doc.source.label,
      sheet.meta.as_of,
    ),
  });
  const summary = lines.lines.length
    ? `${c.label}: monthly ${FLOW_WORD[flow]} of ${name} by sub-component, ${first} to ${last}, indexed to 100 in ${first}. ` +
      lines.lines
        .map((l) => {
          const pts = l.points.filter((p) => Number.isFinite(p.y));
          const base = pts[0]?.y ?? 0;
          const end = pts[pts.length - 1]?.y ?? 0;
          return `${l.label} ${base > 0 ? ((100 * end) / base).toFixed(0) : '–'}`;
        })
        .join('; ') + '.'
    : `${c.label}: no indexable monthly ${FLOW_WORD[flow]} of ${name}.`;
  return (
    <div ref={ref} data-testid="trade-monthly">
      <ChartCard
        headingLevel={3}
        title={`${c.label}: monthly ${FLOW_WORD[flow]} by sub-component`}
        sub={`${cap(FLOW_WORD[flow])} of ${name} as it reported them to UN Comtrade, ${first} to ${last} (its latest 12 months); each line is indexed to 100 in ${first}. Monthly figures are as reported, with no mirror flows, so they do not match the annual trade elsewhere on this page.`}
        tools={
          <div className="flex flex-wrap items-end gap-3">
            <Segmented label="Flow" hideLabel value={flow} options={[{ value: 'imports', label: 'Imports' }, { value: 'exports', label: 'Exports' }]} onChange={setFlow} />
            <Select label="Sector" hideLabel value={c.key} options={doc.cells.map((x) => ({ value: x.key, label: x.label }))} onChange={(k) => setCell(k)} className="min-w-[14rem]" />
          </div>
        }
        summary={summary}
        csv={csv}
        table={() => <TableView rows={rows} cols={cols} caption={`${cap(name)}: ${c.label}, monthly ${FLOW_WORD[flow]}`} rowKey={(r) => `${r.sub_component}|${r.month}`} pageSize={12} />}
        notes={notes.length ? <ul className="list-disc space-y-1 pl-5">{notes.map((n) => <li key={n}>{n}</li>)}</ul> : undefined}
      >
        {lines.lines.length ? (
          <LabelledLines
            lines={lines.lines}
            index
            indexLabel={`${first} = 100`}
            rule={100}
            log={log}
            yLabel="Index"
            ariaLabel={summary}
            valueFormat={fmtUsd}
            xTickFormat={(x) => monthTick(doc.window.months, x)}
            xTickSpacing={90}
            xLabelFormat={(x) => monthLabel(doc.window.months[Math.round(x)] ?? '')}
          />
        ) : (
          <p className="text-sm text-paper-mute">No sub-component of {c.label} has {FLOW_WORD[flow]} in {first} to index from.</p>
        )}
      </ChartCard>
    </div>
  );
}
