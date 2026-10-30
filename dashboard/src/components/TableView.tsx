// Table twin and ranking table (spec_react.md 8.8): TanStack Table for sorting, searching and
// pagination; sticky header; tabular numbers; text cells inserted as text, never HTML.

import { useMemo, useState, type ReactNode } from 'react';
import {
  flexRender, getCoreRowModel, getFilteredRowModel, getPaginationRowModel, getSortedRowModel, useReactTable,
  type ColumnDef, type Row, type SortingState,
} from '@tanstack/react-table';
import { DASH } from '../lib/format.ts';

export type CellValue = string | number | boolean | null | undefined;

export interface Col<R> {
  id: string;
  header: string;
  value: (r: R) => CellValue;
  /** rendered cell (default: the value as text) */
  cell?: (r: R) => ReactNode;
  num?: boolean;
  sortable?: boolean;
  /** the key the column sorts by, when it is not the value (a stage sorts in chain order: stageRank) */
  sortValue?: (r: R) => number;
  minWidth?: number;
  title?: string;
}

export interface TableViewProps<R> {
  rows: R[];
  cols: Col<R>[];
  caption: string;
  search?: boolean;
  pageSize?: number;
  initialSort?: { id: string; desc?: boolean };
  rowKey?: (r: R) => string;
  onRowClick?: (r: R) => void;
  selectedKey?: string | null;
  /** a vertical scroll limit (default none: pages, never a scrolling card) */
  maxHeight?: number;
  empty?: string;
  /** let the column headers wrap (at their words, bottom-aligned), so a wide table of long headers fits its card */
  wrapHeaders?: boolean;
}

const text = (v: CellValue): string => (v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v)) ? DASH : typeof v === 'boolean' ? (v ? 'yes' : 'no') : String(v));

export function TableView<R>({
  rows, cols, caption, search, pageSize = 15, initialSort, rowKey, onRowClick, selectedKey, maxHeight, empty = 'No rows for this selection', wrapHeaders = false,
}: TableViewProps<R>) {
  const [sorting, setSorting] = useState<SortingState>(initialSort ? [{ id: initialSort.id, desc: !!initialSort.desc }] : []);
  const [filter, setFilter] = useState('');
  const columns = useMemo<ColumnDef<R>[]>(
    () =>
      cols.map((c) => ({
        id: c.id,
        header: c.header,
        // NA as undefined, so sortUndefined: 'last' keeps it at the bottom in both directions
        accessorFn: (r: R) => {
          const v = c.value(r);
          return v === null || (typeof v === 'number' && !Number.isFinite(v)) ? undefined : v;
        },
        enableSorting: c.sortable !== false,
        sortUndefined: 'last' as const,
        sortingFn: c.sortValue
          ? (a: Row<R>, b: Row<R>) => c.sortValue!(a.original) - c.sortValue!(b.original)
          : c.num ? ('basic' as const) : ('alphanumeric' as const),
        cell: (info) => (c.cell ? c.cell(info.row.original) : text(info.getValue() as CellValue)),
        meta: c,
      })),
    [cols],
  );
  const table = useReactTable({
    data: rows,
    columns,
    state: { sorting, globalFilter: filter },
    onSortingChange: setSorting,
    onGlobalFilterChange: setFilter,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    globalFilterFn: 'includesString',
    initialState: { pagination: { pageSize, pageIndex: 0 } },
    autoResetPageIndex: true,
  });
  const n = table.getFilteredRowModel().rows.length;
  const pages = table.getPageCount();
  return (
    <div>
      {search && rows.length > 10 && (
        <div className="mb-3 flex items-center gap-3">
          <label className="sr-only" htmlFor={`${caption}-search`}>
            Search {caption}
          </label>
          <input
            id={`${caption}-search`}
            type="search"
            className="field field-sm max-w-xs"
            placeholder="Search"
            value={filter}
            onChange={(e) => setFilter(e.target.value)}
          />
          <span className="text-xs text-paper-mute">
            {n} of {rows.length} rows
          </span>
        </div>
      )}
      <div className={`border hairline ${maxHeight ? 'overflow-auto' : 'overflow-x-auto'}`} style={maxHeight ? { maxHeight } : undefined}>
        <table className="data-table">
          <caption className="sr-only">{caption}</caption>
          <thead>
            {table.getHeaderGroups().map((hg) => (
              <tr key={hg.id}>
                {hg.headers.map((h) => {
                  const meta = h.column.columnDef.meta as Col<R>;
                  const sorted = h.column.getIsSorted();
                  return (
                    <th
                      key={h.id}
                      className={meta.num ? 'num' : undefined}
                      style={wrapHeaders ? { minWidth: meta.minWidth, whiteSpace: 'normal', verticalAlign: 'bottom' } : { minWidth: meta.minWidth }}
                      aria-sort={sorted === 'asc' ? 'ascending' : sorted === 'desc' ? 'descending' : 'none'}
                      title={meta.title}
                    >
                      {h.column.getCanSort() ? (
                        <button
                          type="button"
                          className="inline-flex items-center gap-1 uppercase hover:text-paper"
                          style={wrapHeaders ? { textAlign: 'inherit' } : undefined}
                          onClick={h.column.getToggleSortingHandler()}
                        >
                          {flexRender(h.column.columnDef.header, h.getContext())}
                          <span aria-hidden="true" className="text-[0.625rem]">
                            {sorted === 'asc' ? '▲' : sorted === 'desc' ? '▼' : ''}
                          </span>
                        </button>
                      ) : (
                        flexRender(h.column.columnDef.header, h.getContext())
                      )}
                    </th>
                  );
                })}
              </tr>
            ))}
          </thead>
          <tbody>
            {table.getRowModel().rows.map((row) => {
              const key = rowKey ? rowKey(row.original) : row.id;
              const sel = selectedKey !== undefined && selectedKey !== null && key === selectedKey;
              return (
                <tr
                  key={key}
                  aria-selected={onRowClick ? sel : undefined}
                  className={onRowClick ? 'cursor-pointer' : undefined}
                  onClick={onRowClick ? () => onRowClick(row.original) : undefined}
                  onKeyDown={
                    onRowClick
                      ? (e) => {
                          if (e.key === 'Enter' || e.key === ' ') {
                            e.preventDefault();
                            onRowClick(row.original);
                          }
                        }
                      : undefined
                  }
                  tabIndex={onRowClick ? 0 : undefined}
                >
                  {row.getVisibleCells().map((cell) => {
                    const meta = cell.column.columnDef.meta as Col<R>;
                    return (
                      <td key={cell.id} className={meta.num ? 'num' : undefined}>
                        {flexRender(cell.column.columnDef.cell, cell.getContext())}
                      </td>
                    );
                  })}
                </tr>
              );
            })}
            {n === 0 && (
              <tr>
                <td colSpan={cols.length} className="py-6 text-center text-paper-mute">
                  {empty}
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {pages > 1 && (
        <div className="mt-2 flex items-center gap-2 text-xs text-paper-mute">
          <button type="button" className="btn-quiet" onClick={() => table.previousPage()} disabled={!table.getCanPreviousPage()}>
            Previous
          </button>
          <span className="tabular">
            Page {table.getState().pagination.pageIndex + 1} of {pages}
          </span>
          <button type="button" className="btn-quiet" onClick={() => table.nextPage()} disabled={!table.getCanNextPage()}>
            Next
          </button>
        </div>
      )}
    </div>
  );
}
