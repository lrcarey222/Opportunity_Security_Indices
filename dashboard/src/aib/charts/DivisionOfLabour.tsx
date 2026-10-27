// The country x node division of labour of a strategic compact (2026-10-20): who does what in the compact.
// Rows are economies, columns the compact's nodes in value-chain order (their step above), each cell 0-3
// dots binned from the export's own evidence on one measure (derive/strategic.ts divisionMatrix() and
// DIVISION_BINS): physical capability (share of world supply), allied demand (share of the allied demand
// scope's imports) or investment host (rank as a host of simulated new capacity). The matrix is a real
// table: each cell's dots are drawn (aria-hidden) and spoken (the value and the evidence flags), so the
// dots are never the only carrier; the card's Table toggle swaps it for its table twin (the values as
// numbers), with a legend saying what one, two and three dots mean and a CSV of every cell. It does not
// imply that every economy should build every node.

import type { ReactNode } from 'react';
import { ChartCard } from '../../charts/ChartCard.tsx';
import { TableView, type Col } from '../../components/TableView.tsx';
import { Segmented } from '../../components/ui.tsx';
import { rowsToCsv } from '../../lib/csv.ts';
import { HAIRLINE, PAPER_DIM } from '../../theme/palette.ts';
import { fmtShare } from '../derive/format.ts';
import {
  DIVISION_MEASURES, divisionCells, divisionIntensity, divisionLegend, divisionMeasureLabel, divisionValueText, type DivisionCell, type DivisionLevel, type DivisionMatrix,
  type DivisionMeasure, type DivisionRow,
} from '../derive/strategic.ts';

/** 0-3 dots, the filled ones first (drawn only: the cell speaks its value). */
export function Dots({ n, max = 3, size = 8 }: { n: DivisionLevel | number; max?: number; size?: number }) {
  const gap = 3;
  const w = max * size + (max - 1) * gap;
  return (
    <svg width={w} height={size} aria-hidden="true" className="inline-block align-middle">
      {Array.from({ length: max }, (_, i) => (
        <circle key={i} cx={i * (size + gap) + size / 2} cy={size / 2} r={size / 2 - 0.75} fill={i < n ? PAPER_DIM : 'none'} stroke={i < n ? PAPER_DIM : HAIRLINE} strokeWidth={1.5} />
      ))}
    </svg>
  );
}

/** What one, two and three dots mean for a measure. */
export function DivisionLegendView({ measure }: { measure: DivisionMeasure }) {
  const l = divisionLegend(measure);
  return (
    <div className="flex flex-col gap-1.5 text-xs text-paper-dim" data-testid="division-legend">
      <p>
        <span className="text-paper">Dots:</span> {l.title}.
      </p>
      <ul className="flex flex-wrap gap-x-4 gap-y-1" aria-label={`What the dots mean: ${divisionMeasureLabel(measure)}`}>
        {l.levels.map((x) => (
          <li key={x.n} className="inline-flex items-center gap-1.5">
            <Dots n={x.n} />
            <span>{x.words}</span>
          </li>
        ))}
      </ul>
      <p className="text-paper-mute">{l.note}</p>
    </div>
  );
}

const FLAG_WORDS: [keyof DivisionCell['flags'], string][] = [
  ['anchor', 'anchor position'],
  ['specialist', 'specialist position'],
  ['host', 'capable host for new capacity it needs'],
  ['momentum', 'gaining share'],
];

// The global `.data-table th` rule (uppercase, nowrap, 11px; unlayered, so it beats the utilities) would make
// the economy and node headers wider than a phone (2026-10-21 review): normal case, wrapping, a sensible
// width. The economy column stays on the left while the dots scroll (sticky, with the header's background).
const COL_TH_STYLE = { textTransform: 'none', letterSpacing: 'normal', fontSize: '0.75rem', fontWeight: 400, whiteSpace: 'normal', minWidth: '5.5rem', maxWidth: '9rem' } as const;
const ROW_TH_STYLE = {
  position: 'sticky', left: 0, zIndex: 1, textTransform: 'none', letterSpacing: 'normal', fontSize: '0.8125rem', fontWeight: 400,
  whiteSpace: 'normal', minWidth: '7.5rem', maxWidth: '11rem',
} as const;
const CORNER_TH_STYLE = { ...ROW_TH_STYLE, zIndex: 2 } as const;
/** A compact role without the non-allied suffix (the row says "non-allied" already). */
const roleWords = (role: string): string => role.replace(/ \(non-allied supplier\)$/, '');

/** The evidence flags of a cell in words ("anchor position, gaining share"; '' when none). */
export const flagWords = (c: DivisionCell | null): string => (c ? FLAG_WORDS.filter(([k]) => c.flags[k]).map(([, w]) => w).join(', ') : '');

/** A cell's words: its measure value and the evidence flags. */
function cellWords(c: DivisionCell | null, measure: DivisionMeasure, level: DivisionLevel): string {
  if (!c || level === 0) return 'no material position';
  const v = measure === 'host' ? divisionValueText(c, 'host') : `${divisionValueText(c, measure)} ${measure === 'supply' ? 'of world supply' : 'of allied demand'}`;
  const f = flagWords(c);
  return `${v}${f ? `; ${f}` : ''}`;
}

export interface DivisionDotsProps {
  matrix: DivisionMatrix;
  name: (iso3: string) => string;
  /** the selected node (its column header is pressed) */
  selectedNode?: string | null;
  /** a node's column header clicked (Enter / Space): select the node */
  onSelectNode?: (nodeId: string) => void;
  /** an economy's page (its name links there), or null for no link */
  rowHref?: (iso3: string) => string | null;
  /** a link renderer for rowHref (the page's router Link); default a plain anchor */
  renderLink?: (href: string, children: ReactNode) => ReactNode;
  caption?: string;
}

/** The dot matrix: a table, one row per economy, one column per node (step above), dots in each cell. */
export function DivisionDots({ matrix, name, selectedNode = null, onSelectNode, rowHref, renderLink, caption }: DivisionDotsProps) {
  const { nodes, rows, measure } = matrix;
  const link = (iso: string, text: string) => {
    const h = rowHref?.(iso) ?? null;
    if (!h) return text;
    return renderLink ? renderLink(h, text) : <a className="link" href={h}>{text}</a>;
  };
  return (
    <div className="overflow-x-auto" data-testid="division-of-labour">
      <table className="data-table w-full text-sm">
        <caption className="sr-only">
          {caption ?? `Division of labour in ${matrix.compact.name}: ${divisionMeasureLabel(measure).toLowerCase()} by economy and node`}
        </caption>
        <thead>
          <tr>
            <th scope="col" className="text-left" style={CORNER_TH_STYLE}>
              Economy
            </th>
            {nodes.map((n) => (
              <th key={n.id} scope="col" className="text-left align-bottom" style={COL_TH_STYLE}>
                <span className="block text-[11px] font-normal text-paper-mute">{n.step}</span>
                {onSelectNode ? (
                  <button type="button" className={`link text-left ${selectedNode === n.id ? 'text-paper underline' : ''}`} aria-pressed={selectedNode === n.id} onClick={() => onSelectNode(n.id)}>
                    {n.label}
                  </button>
                ) : (
                  n.label
                )}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((r) => (
            <tr key={r.iso3}>
              <th scope="row" className="text-left font-normal" style={ROW_TH_STYLE}>
                <span className="text-paper">{link(r.iso3, name(r.iso3))}</span>
                {r.allied === false && <span className="ml-1.5 text-xs text-paper-mute">non-allied</span>}
                {r.role && <span className="block text-xs text-paper-mute">{roleWords(r.role)}</span>}
              </th>
              {r.cells.map((c, i) => {
                const lv = r.levels[i];
                const words = cellWords(c, measure, lv);
                return (
                  <td key={nodes[i].id} title={`${name(r.iso3)}, ${nodes[i].label}: ${words}`} className={selectedNode === nodes[i].id ? 'bg-white/5' : undefined}>
                    <Dots n={lv} />
                    <span className="sr-only">{words}</span>
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

/** The table twin: the measure's values as numbers (sortable), with the economy's role. */
export function DivisionTable({ matrix, name }: { matrix: DivisionMatrix; name: (iso3: string) => string }) {
  const { nodes, measure } = matrix;
  const value = (r: DivisionRow, i: number): number | null => {
    const c = r.cells[i];
    if (!c) return null;
    return measure === 'supply' ? c.supply : measure === 'demand' ? c.demand : c.hostRank;
  };
  const cols: Col<DivisionRow>[] = [
    { id: 'economy', header: 'Economy', value: (r) => name(r.iso3), minWidth: 140 },
    { id: 'allied', header: 'Allied', value: (r) => (r.allied === null ? null : r.allied) },
    { id: 'role', header: 'Role in the compact', value: (r) => r.role, minWidth: 160 },
    ...nodes.map(
      (n, i): Col<DivisionRow> => ({
        id: `n-${n.id}`,
        header: `${n.label} (${n.step})`,
        value: (r) => value(r, i),
        cell: (r) => (measure === 'host' ? divisionValueText(r.cells[i], 'host') : fmtShare(value(r, i), 0)),
        num: true,
        title: divisionLegend(measure).title,
      }),
    ),
  ];
  return <TableView rows={matrix.rows} cols={cols} caption={`${matrix.compact.name}: ${divisionMeasureLabel(measure).toLowerCase()} by economy and node`} rowKey={(r) => r.iso3} />;
}

/** Every cell of the compact's division of labour as CSV rows (every measure, the flags spelt out). */
export function divisionCsvRows(matrix: DivisionMatrix, name: (iso3: string) => string, nodeLabel: (id: string) => string): Record<string, unknown>[] {
  const step = new Map(matrix.nodes.map((n) => [n.id, n.step]));
  return divisionCells(matrix.compact).map((c) => ({
    strategic_compact_id: matrix.compact.id,
    iso3: c.iso3,
    economy: name(c.iso3),
    node_id: c.node,
    node: nodeLabel(c.node),
    value_chain_step: step.get(c.node) ?? null,
    share_of_world_supply: c.supply,
    share_of_allied_demand: c.demand,
    best_host_rank: c.hostRank,
    anchor_position: c.flags.anchor,
    specialist_position: c.flags.specialist,
    top_capable_host: c.flags.host,
    momentum: c.flags.momentum,
    [`dots_${matrix.measure}`]: divisionIntensity(c, matrix.measure),
  }));
}

export interface DivisionOfLabourProps extends Omit<DivisionDotsProps, 'caption'> {
  title?: string;
  sub?: ReactNode;
  headingLevel?: 2 | 3;
  badge?: ReactNode;
  /** offer the measure choice (the parent recomputes the matrix with divisionMatrix()) */
  onMeasure?: (m: DivisionMeasure) => void;
  /** node labels for the CSV (default: the matrix's) */
  nodeLabel?: (id: string) => string;
  /** the CSV file name (default "<compact id> division of labour <measure>") */
  csvName?: string;
}

/** The division of labour as a card: the measure choice, the dot matrix (Table: its twin), the legend and a CSV. */
export function DivisionOfLabour({ matrix, name, title = 'Who does what', sub, headingLevel = 2, badge, onMeasure, nodeLabel, csvName, ...dots }: DivisionOfLabourProps) {
  const label = (id: string) => nodeLabel?.(id) ?? matrix.nodes.find((n) => n.id === id)?.label ?? id;
  const measures = DIVISION_MEASURES.map((m) => ({ value: m, label: divisionMeasureLabel(m) }));
  const summary = `${matrix.compact.name}: ${divisionMeasureLabel(matrix.measure).toLowerCase()} of ${matrix.rows.length} economies across ${matrix.nodes.length} nodes.`;
  return (
    <ChartCard
      title={title}
      sub={sub}
      headingLevel={headingLevel}
      badge={badge}
      summary={summary}
      tools={onMeasure ? <Segmented label="Dots show" value={matrix.measure} options={measures} onChange={onMeasure} hideLabel /> : undefined}
      legend={<DivisionLegendView measure={matrix.measure} />}
      table={() => <DivisionTable matrix={matrix} name={name} />}
      notes={matrix.hidden > 0 ? `${matrix.hidden} more ${matrix.hidden === 1 ? 'economy has' : 'economies have'} a smaller position; the CSV lists every one.` : undefined}
      csv={() => ({ name: csvName ?? `${matrix.compact.id} division of labour ${matrix.measure}`, text: rowsToCsv(divisionCsvRows(matrix, name, label)) })}
    >
      <DivisionDots matrix={matrix} name={name} {...dots} />
    </ChartCard>
  );
}
