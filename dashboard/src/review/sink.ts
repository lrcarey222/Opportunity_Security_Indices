// Where a finished submission goes (docs/QC_SYSTEM_DESIGN.md 5 step 1, 7). Three sinks, chosen at build
// time by VITE_REVIEW_SINK (default 'local'):
// - local:  stays in this browser (storage.ts); the reviewer can download a packet later.
// - packet: also downloads a JSON packet the reviewer sends to NEIS by hand.
// - http:   POSTs the JSON to VITE_REVIEW_ENDPOINT (a public, write-only URL; no credentials, no cookies,
//           a 15 s timeout). The receiving service validates, rate-limits and stores; the CSP must allow it.
// No secrets, no third-party SDK, and payloads are never logged.

import type { Submission } from './model.ts';
import { downloadText } from '../lib/download.ts';
import { pseudonymReviewerCode } from './privacy.ts';
import { reviewStorage, type ReviewStorage } from './storage.ts';

export type SinkMode = 'local' | 'packet' | 'http';

export interface SinkResult {
  ok: boolean;
  id: string;
  message: string;
}

export interface SubmissionSink {
  mode: SinkMode;
  submit(sub: Submission): Promise<SinkResult>;
}

export const PACKET_SCHEMA = 'aib-review-packet';

export interface ReviewPacket {
  schema: typeof PACKET_SCHEMA;
  schema_version: 1;
  reviewer_id: string;
  submissions: Submission[];
}

/** A packet of one reviewer's submissions (the reviewer id of the first; mixed reviewers are refused). */
export function buildPacket(subs: readonly Submission[]): ReviewPacket {
  if (subs.length === 0) throw new Error('a packet needs at least one submission');
  const rid = subs[0].reviewer.reviewer_id;
  if (subs.some((s) => s.reviewer.reviewer_id !== rid)) throw new Error('a packet holds one reviewer’s submissions');
  return { schema: PACKET_SCHEMA, schema_version: 1, reviewer_id: rid, submissions: [...subs] };
}

/** The packet's file name: pseudonymous code and the submission time (no name, no email). */
export function packetFileName(p: ReviewPacket): string {
  const last = p.submissions.map((s) => s.submitted_at ?? s.created_at).sort().at(-1) ?? 'undated';
  return `aib-review-${pseudonymReviewerCode(p.reviewer_id)}-${last.slice(0, 19).replace(/[^0-9T]/g, '')}.json`;
}

const stamp = (sub: Submission): Submission => ({ ...sub });

export class LocalSink implements SubmissionSink {
  readonly mode = 'local' as const;
  private readonly store: ReviewStorage;
  constructor(store: ReviewStorage = reviewStorage()) {
    this.store = store;
  }
  async submit(sub: Submission): Promise<SinkResult> {
    const saved = this.store.saveSubmission(stamp(sub));
    return saved
      ? { ok: true, id: sub.submission_id, message: 'Saved in this browser. Nothing has been sent.' }
      : { ok: true, id: sub.submission_id, message: 'Kept for this visit only (browser storage is unavailable): download a packet before you leave.' };
  }
}

export class PacketSink implements SubmissionSink {
  readonly mode = 'packet' as const;
  private readonly store: ReviewStorage | null;
  private readonly download: (text: string, filename: string, type?: string) => void;
  constructor(opts: { store?: ReviewStorage | null; download?: (text: string, filename: string, type?: string) => void } = {}) {
    this.store = opts.store === undefined ? reviewStorage() : opts.store;
    this.download = opts.download ?? downloadText;
  }
  async submit(sub: Submission): Promise<SinkResult> {
    try {
      this.store?.saveSubmission(stamp(sub));
      const packet = buildPacket([sub]);
      this.download(JSON.stringify(packet, null, 2), packetFileName(packet), 'application/json');
      return { ok: true, id: sub.submission_id, message: 'A packet file was downloaded: send it to NEIS.' };
    } catch (e) {
      return { ok: false, id: sub.submission_id, message: `The packet could not be built (${e instanceof Error ? e.message : 'unknown error'}).` };
    }
  }
}

type FetchLike = (url: string, init: RequestInit) => Promise<{ ok: boolean; status: number }>;

export class HttpSink implements SubmissionSink {
  readonly mode = 'http' as const;
  private readonly endpoint: string | null;
  private readonly fetcher: FetchLike;
  private readonly timeoutMs: number;
  constructor(opts: { endpoint?: string | null; fetcher?: FetchLike; timeoutMs?: number } = {}) {
    this.endpoint = opts.endpoint ?? null;
    this.fetcher = opts.fetcher ?? ((u, i) => fetch(u, i));
    this.timeoutMs = opts.timeoutMs ?? 15_000;
  }
  async submit(sub: Submission): Promise<SinkResult> {
    if (!this.endpoint || !/^https?:\/\//.test(this.endpoint)) return { ok: false, id: sub.submission_id, message: 'No review endpoint is configured for this build.' };
    const ctl = typeof AbortController === 'function' ? new AbortController() : null;
    const timer = ctl ? setTimeout(() => ctl.abort(), this.timeoutMs) : null;
    try {
      const res = await this.fetcher(this.endpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(sub),
        credentials: 'omit',
        mode: 'cors',
        cache: 'no-store',
        referrerPolicy: 'no-referrer',
        ...(ctl ? { signal: ctl.signal } : {}),
      });
      return res.ok
        ? { ok: true, id: sub.submission_id, message: 'Sent to NEIS.' }
        : { ok: false, id: sub.submission_id, message: `The endpoint refused the submission (HTTP ${res.status}); it is kept in this browser.` };
    } catch (e) {
      const aborted = (e as { name?: string })?.name === 'AbortError';
      return { ok: false, id: sub.submission_id, message: aborted ? 'The endpoint did not answer in time; the submission is kept in this browser.' : 'The submission could not be sent; it is kept in this browser.' };
    } finally {
      if (timer) clearTimeout(timer);
    }
  }
}

/** The build's sink mode (VITE_REVIEW_SINK; anything unknown is 'local'). */
export function sinkMode(env: Record<string, unknown> | undefined = import.meta.env): SinkMode {
  const m = env?.VITE_REVIEW_SINK;
  return m === 'packet' || m === 'http' ? m : 'local';
}

/** The sink of this build. An http sink also keeps a local copy first, so nothing is lost when the POST fails. */
export function getSink(env: Record<string, unknown> | undefined = import.meta.env, store?: ReviewStorage): SubmissionSink {
  const mode = sinkMode(env);
  if (mode === 'packet') return new PacketSink({ store: store ?? reviewStorage() });
  if (mode === 'http') {
    const http = new HttpSink({ endpoint: typeof env?.VITE_REVIEW_ENDPOINT === 'string' ? env.VITE_REVIEW_ENDPOINT : null });
    const local = new LocalSink(store ?? reviewStorage());
    return {
      mode: 'http',
      async submit(sub) {
        await local.submit(sub);
        return http.submit(sub);
      },
    };
  }
  return new LocalSink(store ?? reviewStorage());
}
