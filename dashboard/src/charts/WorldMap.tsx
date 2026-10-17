// Map frames drawn with d3-geo into React SVG (exact per-country hover and click): an Equal Earth
// projection fitted to the width, countries filled by a value or hatched (missing / not rankable /
// excluded, with the reason in the tooltip), an orange outline on the selected and hovered country.
// Overlays (links and nodes) are drawn by the caller through `children(projection)`.
// Additive for Sector Insights (spec_sector.md 3.5, 5.2): a cell may be an observed zero ('zero',
// solid ink-600), carry an orange `outline` (the highlighted economy) and be `dim` (40% opacity,
// out of the selected scope).
// Additive for the three-colour scale (spec_country.md C1.4): `ringStyle="double"` draws the selected,
// outlined and hovered countries with an ink-under-white double ring instead of the orange outline
// (orange is the low end of the scale and would vanish there). Default 'orange': nothing else changes.

import { useEffect, useMemo, useRef, useState, type ReactNode } from 'react';
import { geoEqualEarth, geoPath, type GeoProjection } from 'd3';
import { loadWorld, type World } from './world.ts';
import { INK, SIGNAL, ZERO_FILL } from '../theme/palette.ts';
import { Loading } from '../components/ui.tsx';
import { ringLayers, type RingKind } from './rings.ts';

export interface MapCell {
  /** a colour, 'hatch' for missing / excluded, or 'zero' for an observed zero */
  fill: string | 'hatch' | 'zero';
  iso3c: string;
  tip: ReactNode;
  /** draw the orange outline (the highlighted economy), as for `selected` */
  outline?: boolean;
  /** 40% opacity (outside the selected scope) */
  dim?: boolean;
}

/** The SVG fill of a map cell. */
export function mapFill(fill: MapCell['fill']): string {
  return fill === 'hatch' ? 'url(#hatch)' : fill === 'zero' ? ZERO_FILL : fill;
}

export function useWorld(): { world: World | null; error: string | null } {
  const [world, setWorld] = useState<World | null>(null);
  const [error, setError] = useState<string | null>(null);
  useEffect(() => {
    let live = true;
    loadWorld().then(
      (w) => live && setWorld(w),
      (e) => live && setError(String(e)),
    );
    return () => {
      live = false;
    };
  }, []);
  return { world, error };
}

export function useWidth<T extends HTMLElement>(): [React.RefObject<T | null>, number] {
  const ref = useRef<T>(null);
  const [w, setW] = useState(0);
  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    const m = () => setW(Math.floor(el.clientWidth));
    m();
    if (typeof ResizeObserver === 'undefined') return;
    const ro = new ResizeObserver(m);
    ro.observe(el);
    return () => ro.disconnect();
  }, []);
  return [ref, w];
}

export interface WorldMapProps {
  /** iso_n3 -> cell; countries without one are drawn as plain land */
  cells: Map<string, MapCell>;
  ariaLabel: string;
  onSelect?: (iso3c: string) => void;
  selected?: string | null;
  /** extra SVG on top (projected with the map's projection) */
  overlay?: (projection: GeoProjection, width: number, height: number) => ReactNode;
  /** tooltip of an overlay element, managed by the caller */
  overlayTip?: { x: number; y: number; content: ReactNode } | null;
  land?: string;
  /** 'orange' (default): the orange outline; 'double': the ink + white ring of three-colour maps */
  ringStyle?: 'orange' | 'double';
}

export function WorldMap({ cells, ariaLabel, onSelect, selected, overlay, overlayTip, land = INK[700], ringStyle = 'orange' }: WorldMapProps) {
  const { world, error } = useWorld();
  const [hostRef, width] = useWidth<HTMLDivElement>();
  const [hover, setHover] = useState<{ key: string; x: number; y: number } | null>(null);
  const height = Math.max(200, Math.round(width * 0.48));
  const geo = useMemo(() => {
    if (!world || width <= 0) return null;
    const projection = geoEqualEarth().fitExtent(
      [
        [4, 4],
        [width - 4, height - 4],
      ],
      world.fit,
    );
    const path = geoPath(projection);
    const paths = world.countries.map((f) => ({ id: String(f.id ?? ''), d: path(f) ?? '' }));
    return { projection, paths };
  }, [world, width, height]);

  const move = (e: React.PointerEvent, key: string) => {
    const r = hostRef.current?.getBoundingClientRect();
    if (!r) return;
    setHover({ key, x: e.clientX - r.left, y: e.clientY - r.top });
  };
  const hoverCell = hover ? cells.get(hover.key) : undefined;
  const tip = overlayTip ?? (hover && hoverCell ? { x: hover.x, y: hover.y, content: hoverCell.tip } : null);
  const flip = tip ? tip.x > width * 0.6 : false;
  const selectedN3 = selected ? [...cells.entries()].find(([, c]) => c.iso3c === selected)?.[0] : undefined;
  // the outline layers of a set of country paths (one orange stroke, or ink under white)
  const ring = (ps: { id: string; d: string }[], kind: RingKind, key: string) =>
    ringLayers(ringStyle, kind, SIGNAL).flatMap((l, j) => ps.map((p, i) => <path key={`${key}-${j}-${i}`} d={p.d} fill="none" stroke={l.stroke} strokeWidth={l.width} strokeLinejoin="round" />));
  const outlined = useMemo(() => new Set([...cells.entries()].filter(([k, c]) => c.outline && k !== selectedN3).map(([k]) => k)), [cells, selectedN3]);

  return (
    <div ref={hostRef} className="relative w-full" style={{ minHeight: height }} onPointerLeave={() => setHover(null)}>
      {error && <p className="text-sm text-alert">The map shapes could not be loaded ({error}).</p>}
      {!geo && !error && <Loading label="Loading the map" />}
      {geo && (
        <svg width={width} height={height} role="img" aria-label={ariaLabel} className="block">
          <defs>
            <pattern id="hatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">
              <rect width="5" height="5" fill={INK[700]} />
              <line x1="0" y1="0" x2="0" y2="5" stroke="rgb(255 255 255 / 0.28)" strokeWidth="1" />
            </pattern>
          </defs>
          <g>
            {geo.paths.map((p, i) => {
              const c = cells.get(p.id);
              const fill = c ? mapFill(c.fill) : land;
              return (
                <path
                  key={`${p.id}-${i}`}
                  d={p.d}
                  fill={fill}
                  stroke="rgb(255 255 255 / 0.18)"
                  strokeWidth={0.5}
                  onPointerMove={c ? (e) => move(e, p.id) : undefined}
                  onPointerLeave={() => setHover(null)}
                  onClick={c && onSelect ? () => onSelect(c.iso3c) : undefined}
                  opacity={c?.dim ? 0.4 : undefined}
                  style={{ cursor: c && onSelect ? 'pointer' : undefined }}
                />
              );
            })}
          </g>
          {outlined.size > 0 && (
            <g pointerEvents="none" data-ring="outline">
              {ring(geo.paths.filter((p) => outlined.has(p.id)), 'selected', 'out')}
            </g>
          )}
          {selectedN3 && (
            <g pointerEvents="none" data-ring="selected">
              {ring(geo.paths.filter((p) => p.id === selectedN3), 'selected', 'sel')}
            </g>
          )}
          {hover && (
            <g pointerEvents="none" data-ring="hover">
              {ring(geo.paths.filter((p) => p.id === hover.key), 'hover', 'hov')}
            </g>
          )}
          {overlay?.(geo.projection, width, height)}
        </svg>
      )}
      {tip && (
        <div
          className="pointer-events-none absolute z-20 max-w-[18rem] border border-signal bg-ink-900 px-3 py-2 text-xs text-paper-dim"
          style={{ left: flip ? undefined : tip.x + 14, right: flip ? width - tip.x + 14 : undefined, top: Math.max(0, tip.y - 10) }}
          role="status"
        >
          {tip.content}
        </div>
      )}
    </div>
  );
}
