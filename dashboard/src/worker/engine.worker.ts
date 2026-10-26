// Web Worker entry (spec_react.md 7.1): wires the EngineHost to postMessage / fetch. Created by
// EngineClient.create() as a module worker. Gzip chunks are inflated with the platform's
// DecompressionStream; fflate is loaded (as a separate chunk) only where that is missing.

import '../artifactFetch.ts';
import { gunzipStream } from '../engine/codec.ts';
import { EngineHost, fetchBytesHttp } from './host.ts';
import type { FromWorker, ToWorker } from './protocol.ts';

// Typed locally instead of through the WebWorker lib, which conflicts with the DOM lib the app uses.
const scope = self as unknown as {
  postMessage(msg: FromWorker, transfer: Transferable[]): void;
  onmessage: ((e: MessageEvent<ToWorker>) => void) | null;
  navigator?: { deviceMemory?: number };
};

async function inflate(bytes: Uint8Array): Promise<Uint8Array> {
  if (typeof DecompressionStream !== 'undefined') {
    try {
      return await gunzipStream(bytes);
    } catch {
      // fall through to fflate
    }
  }
  const { gunzipSync } = await import('fflate');
  return gunzipSync(bytes);
}

// Small devices keep fewer FS / PO cell arrays (12.7 MB each; the baseline's are pinned on top).
const lowMemory = (scope.navigator?.deviceMemory ?? 8) <= 4;
const host = new EngineHost((msg, transfer) => scope.postMessage(msg, transfer), fetchBytesHttp, inflate, {
  cacheLimits: lowMemory ? { fs_cells: 2, po_cells: 2 } : {},
});

scope.onmessage = (e) => {
  void host.handle(e.data);
};
