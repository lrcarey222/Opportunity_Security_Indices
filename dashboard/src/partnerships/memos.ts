// The shipped partnership and compact memos (data/partnerships/manifest.json, scripts/35 and R/partnerships):
// which memo a pair or a Strategic Sectoral Compact has, where its PDF and Word files are, and one line on
// what the memo holds. The manifest lists pair memos under `pairs` (USA_JPN: country_a, country_b), compact
// memos under `compacts` (SC-SOLAR) and group memos under `groups`. Pure (no IO); the fetch is memoStore.ts.

export interface MemoEntry {
  title?: string;
  country_a?: string;
  country_b?: string;
  compact?: string;
  members?: string[];
  json?: string;
  pdf?: string | null;
  docx?: string | null;
  words?: number | null;
  generated_at?: string | null;
  as_of?: string | null;
  format?: string | null;
  verified?: boolean | null;
}

export interface MemoManifest {
  schema: string;
  pairs: Record<string, MemoEntry>;
  compacts: Record<string, MemoEntry>;
  groups: Record<string, MemoEntry>;
}

export const MEMO_SCHEMA = 'osi-partnership-memos';

const isObj = (x: unknown): x is Record<string, unknown> => !!x && typeof x === 'object' && !Array.isArray(x);

/** The manifest when it is one (else null: a missing or foreign file shows no memo action). */
export function readMemoManifest(x: unknown): MemoManifest | null {
  if (!isObj(x) || x.schema !== MEMO_SCHEMA) return null;
  const part = (k: string): Record<string, MemoEntry> => (isObj(x[k]) ? (x[k] as Record<string, MemoEntry>) : {});
  return { schema: MEMO_SCHEMA, pairs: part('pairs'), compacts: part('compacts'), groups: part('groups') };
}

/** A memo with a file to download (a PDF or a Word file). */
const downloadable = (e: MemoEntry | undefined | null): MemoEntry | null => (e && (e.pdf || e.docx) ? e : null);

/** The pair's memo, in either order (USA_JPN for Japan and the United States). */
export function pairMemoOf(m: MemoManifest | null | undefined, a: string | null | undefined, b: string | null | undefined): MemoEntry | null {
  if (!m || !a || !b) return null;
  const A = a.toUpperCase();
  const B = b.toUpperCase();
  for (const [key, e] of Object.entries(m.pairs)) {
    const ca = (e.country_a ?? key.split('_')[0] ?? '').toUpperCase();
    const cb = (e.country_b ?? key.split('_')[1] ?? '').toUpperCase();
    if ((ca === A && cb === B) || (ca === B && cb === A)) return downloadable(e);
  }
  return null;
}

/** A Strategic Sectoral Compact's memo (SC-SOLAR). */
export function compactMemoOf(m: MemoManifest | null | undefined, id: string | null | undefined): MemoEntry | null {
  if (!m || !id) return null;
  return downloadable(m.compacts[id.toUpperCase()]);
}

/** "17 November 2026" from "2026-11-17" or a timestamp (null when unreadable). */
export function memoDate(iso: string | null | undefined): string | null {
  const x = /^(\d{4})-(\d{2})-(\d{2})/.exec(iso ?? '');
  if (!x) return null;
  const months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
  const mo = months[Number(x[2]) - 1];
  return mo ? `${Number(x[3])} ${mo} ${x[1]}` : null;
}

/** What the memo holds (the sections the memo template writes; the same for every memo of the kind). */
export const MEMO_CONTENTS: Readonly<Record<'pair' | 'compact', string>> = {
  pair: 'A decision brief on the partnership: what each country is good at and needs, the trade and investment agenda, recommended Sectoral Compacts, a 12–36 month deal pipeline and what not to do, with an evidence register.',
  compact:
    'A decision brief on the compact: who is good at what and who needs what, the division of labor, the trade and investment agenda, governance, a 12–36 month deal pipeline and what not to do, with an evidence register.',
};

/**
 * The memo's provenance line: when it was written, whether it was checked, and whether it predates the data
 * the page shows (`dataCreated`: the data release the page reads, an ISO date or timestamp).
 */
export function memoProvenance(e: MemoEntry, dataCreated?: string | null): string {
  const written = memoDate(e.as_of ?? e.generated_at);
  const parts = [written ? `Written ${written} from the Explorer data and a dated evidence layer` : 'Written from the Explorer data and a dated evidence layer'];
  if (e.verified === false) parts.push('not independently verified');
  let out = `${parts.join('; ')}.`;
  const at = (e.generated_at ?? e.as_of ?? '').slice(0, 10);
  const rel = (dataCreated ?? '').slice(0, 10);
  if (/^\d{4}-\d{2}-\d{2}$/.test(at) && /^\d{4}-\d{2}-\d{2}$/.test(rel) && at < rel) {
    out += ` It predates the current data release (${memoDate(rel)}), so some figures may differ from this page.`;
  }
  return out;
}
