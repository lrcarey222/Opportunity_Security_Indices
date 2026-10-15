// Legends of the Sector kit (spec_sector.md 3.4, 3.5, 5.3). GroupLegend always names China and the
// residual ("Not attributed", hatched); StateLegend lists only the value states present, in words and
// texture (meaning never rests on colour alone).

import { GROUP_COLOR, HATCH_STYLE, SIGNAL, TRI, ZERO_FILL } from '../../theme/palette.ts';
import { groupLabel, SPLIT_ORDER } from './color.ts';
import type { GroupLegendProps, StateLegendProps, ValueState } from './types.ts';

type Swatch = 'rect' | 'hatch' | 'zero' | 'ramp' | 'hollow' | 'outline' | 'dashed' | 'badge';

function Key({ kind, color }: { kind: Swatch; color?: string }) {
  switch (kind) {
    case 'hatch':
      return <span className="inline-block h-2.5 w-3.5" style={HATCH_STYLE} data-hatch="" />;
    case 'zero':
      return <span className="inline-block h-2.5 w-3.5" style={{ background: ZERO_FILL }} />;
    case 'ramp':
      // the three-colour scale as hard steps (orange low, white median, blue high)
      return (
        <span className="inline-flex h-2.5 w-9">
          {TRI.map((c) => (
            <span key={c} className="block h-full flex-1" style={{ background: c }} />
          ))}
        </span>
      );
    case 'hollow':
      return <span className="inline-block h-2.5 w-2.5 rounded-full border-2" style={{ borderColor: color ?? '#d1d5db' }} />;
    case 'outline':
      return <span className="inline-block h-2.5 w-3.5 border-2" style={{ borderColor: color ?? SIGNAL }} />;
    case 'dashed':
      return <span className="inline-block h-2.5 w-3.5 border-2 border-dashed" style={{ borderColor: color ?? '#d1d5db' }} />;
    case 'badge':
      return <span className="tag px-1 py-0 text-[9px] leading-tight">abc</span>;
    default:
      return <span className="inline-block h-2.5 w-3.5" style={{ background: color }} />;
  }
}

function Items({ items, label }: { items: { label: string; kind: Swatch; color?: string }[]; label: string }) {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label={label}>
      {items.map((it) => (
        <li key={it.label} className="inline-flex items-center gap-1.5">
          <span aria-hidden="true" className="inline-flex min-w-4 items-center justify-center">
            <Key kind={it.kind} color={it.color} />
          </span>
          {it.label}
        </li>
      ))}
    </ul>
  );
}

export function GroupLegend({ groups, highlight, projection, estimate, euBloc, mixed }: GroupLegendProps) {
  const gs = SPLIT_ORDER.filter((g) => !groups || groups.includes(g));
  const items: { label: string; kind: Swatch; color?: string }[] = gs.map((g) => (g === 'unattributed' ? { label: groupLabel(g), kind: 'hatch' } : { label: groupLabel(g), kind: 'rect', color: GROUP_COLOR[g] }));
  // a region whose members span groups (North America holds Mexico, a partner) is counted as not attributed
  if (mixed) items.push({ label: 'Mixed region (not attributed)', kind: 'hatch' });
  if (highlight) items.push({ label: `${highlight} (highlighted)`, kind: 'outline', color: SIGNAL });
  if (projection) items.push({ label: 'Projection (outlined)', kind: 'dashed' });
  if (estimate) items.push({ label: 'Includes an estimate (hollow)', kind: 'hollow' });
  return (
    <div className="space-y-1">
      <Items items={items} label="Groups" />
      {euBloc && <p className="text-xs text-paper-mute">EU-27 counted as one.</p>}
    </div>
  );
}

const STATE_ITEM: Record<ValueState, (source: string) => { label: string; kind: Swatch; color?: string }> = {
  obs: () => ({ label: 'Observed', kind: 'ramp' }),
  zero: () => ({ label: 'Reported zero', kind: 'zero' }),
  nsr: (s) => ({ label: `Not reported separately by ${s}`, kind: 'hatch' }),
  nq: () => ({ label: 'Listed without a figure (withheld, or under 1% / not reported)', kind: 'hatch' }),
  mirror: () => ({ label: "Mirror: from partners' declarations (×1.06)", kind: 'badge' }),
  est: () => ({ label: 'Includes an estimate (hollow marker)', kind: 'hollow' }),
  na_growth: () => ({ label: 'Base year zero ("new" or "n/a")', kind: 'hatch' }),
  not_covered: (s) => ({ label: `Not covered by ${s}`, kind: 'hatch' }),
  regional: () => ({ label: 'Regional value: see the regional panel', kind: 'outline', color: '#d1d5db' }),
  old: () => ({ label: 'Older observation (year shown)', kind: 'hollow' }),
};

const STATE_ORDER: ValueState[] = ['obs', 'zero', 'mirror', 'est', 'old', 'nsr', 'nq', 'na_growth', 'not_covered', 'regional'];

export function StateLegend({ states, source = 'the source' }: StateLegendProps) {
  const present = STATE_ORDER.filter((s) => states.includes(s));
  if (!present.length) return null;
  return <Items items={present.map((s) => STATE_ITEM[s](source))} label="Value states" />;
}
