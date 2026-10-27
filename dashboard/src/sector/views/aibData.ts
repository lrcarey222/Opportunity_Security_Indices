// The Allied Industrial Base document as the Sector pages read it (read-only reuse of aib/data/hooks.ts:
// one fetch, cached by the AIB store). Null while it loads or when the bundle is absent: every Sector
// view renders without it.

import type { AibDoc } from '../../aib/contract.ts';
import { useAibManifest, useCompactsDoc } from '../../aib/data/hooks.ts';

export function useAibDoc(): AibDoc | null {
  const manifest = useAibManifest();
  return useCompactsDoc(manifest.data).data;
}
