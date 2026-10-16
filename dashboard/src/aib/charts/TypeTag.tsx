// The compact type as a tag. "Compact unnecessary" is muted (dashed border, muted text) so it reads as
// a finding about the group, not as a compact; the explanation is the tag's title and is printed beside
// it wherever the type is shown.

import type { TypeInfo } from '../derive/types.ts';

export function TypeTag({ info }: { info: TypeInfo }) {
  const muted = info.tone === 'unnecessary';
  return (
    <span className="tag" title={info.explanation} data-tone={info.tone} style={muted ? { borderStyle: 'dashed', color: 'var(--color-paper-mute)' } : undefined}>
      {info.short}
    </span>
  );
}
