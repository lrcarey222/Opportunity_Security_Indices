// CSV of a columnar table (spec_react.md 8.6, view 8): UTF-8 with a BOM, the R column names,
// numbers in shortest round-trip form (String(x)), NA empty, logicals TRUE / FALSE as R writes them.

import type { Column, Table } from '../engine/types.ts';
import { NA_BOOL, NA_INT } from '../engine/numeric.ts';

const BOM = '﻿';

function cell(c: Column, i: number): string {
  if (Array.isArray(c)) {
    const v = c[i];
    return v === null || v === undefined ? '' : quote(v);
  }
  if (c instanceof Float64Array) {
    const v = c[i];
    return v !== v ? '' : String(v);
  }
  if (c instanceof Int32Array) return c[i] === NA_INT ? '' : String(c[i]);
  const v = (c as Uint8Array)[i];
  return v === NA_BOOL ? '' : v ? 'TRUE' : 'FALSE';
}

function quote(s: string): string {
  return /[",\r\n]/.test(s) || /^\s|\s$/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

/** A table as CSV text (no BOM). */
export function tableToCsv(t: Table, cols?: string[]): string {
  const names = cols ?? Object.keys(t.cols);
  const lines: string[] = [names.map(quote).join(',')];
  for (let i = 0; i < t.nrow; i++) lines.push(names.map((k) => cell(t.cols[k], i)).join(','));
  return lines.join('\r\n') + '\r\n';
}

/** Rows of plain objects as CSV text (no BOM). */
export function rowsToCsv(rows: Record<string, unknown>[], cols?: string[]): string {
  const names = cols ?? (rows.length ? Object.keys(rows[0]) : []);
  const fmt = (v: unknown) =>
    v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v))
      ? ''
      : typeof v === 'boolean'
        ? v
          ? 'TRUE'
          : 'FALSE'
        : typeof v === 'number'
          ? String(v)
          : quote(String(v));
  const lines = [names.map(quote).join(','), ...rows.map((r) => names.map((k) => fmt(r[k])).join(','))];
  return lines.join('\r\n') + '\r\n';
}

export const withBom = (csv: string): string => BOM + csv;
