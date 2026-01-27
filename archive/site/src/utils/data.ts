export type CompactTable<T> = {
  columns: string[];
  rows: Array<Array<unknown>>;
};

export const parseCompactTable = <T extends Record<string, unknown>>(
  payload: CompactTable<T>
): T[] => {
  return payload.rows.map((row) => {
    const entry: Record<string, unknown> = {};
    payload.columns.forEach((column, index) => {
      entry[column] = row[index];
    });
    return entry as T;
  });
};

export const ensureNumber = (value: unknown): number | null => {
  if (value === null || value === undefined || value === "") {
    return null;
  }
  const numeric = Number(value);
  if (Number.isNaN(numeric)) {
    return null;
  }
  return numeric;
};
