import { useMemo, useState } from "react";

type MapViewProps = {
  geoJson: GeoJsonFeatureCollection | null;
  valuesByIso3: Map<string, { value: number | null; country: string }>;
  selectedIso3: string | null;
  hoveredIso3: string | null;
  hoveredCountry: { value: number | null; country: string } | null;
  indexLabel: string;
  onSelectIso3: (iso3: string | null) => void;
  onHoverIso3: (iso3: string | null) => void;
};

type GeoJsonFeatureCollection = {
  type: "FeatureCollection";
  features: GeoJsonFeature[];
};

type GeoJsonFeature = {
  type: "Feature";
  properties: {
    iso3?: string;
    name?: string;
  };
  geometry: GeoJsonGeometry;
};

type GeoJsonGeometry = {
  type: "Polygon" | "MultiPolygon";
  coordinates: number[][][] | number[][][][];
};

const COLORS = ["#0f2d4c", "#18486e", "#236491", "#2e81b5", "#40c2ff"];

const getQuantile = (sorted: number[], quantile: number) => {
  const position = (sorted.length - 1) * quantile;
  const base = Math.floor(position);
  const rest = position - base;
  const next = sorted[base + 1];
  if (next === undefined) {
    return sorted[base];
  }
  return sorted[base] + rest * (next - sorted[base]);
};

const buildQuantiles = (values: number[]) => {
  if (values.length === 0) {
    return [];
  }
  const sorted = [...values].sort((a, b) => a - b);
  return [0, 0.2, 0.4, 0.6, 0.8, 1].map((q) => getQuantile(sorted, q));
};

const formatValue = (value: number | null) => {
  if (value === null || Number.isNaN(value)) {
    return "–";
  }
  return value.toFixed(1);
};

const project = (longitude: number, latitude: number, width: number, height: number) => {
  const x = ((longitude + 180) / 360) * width;
  const y = ((90 - latitude) / 180) * height;
  return [x, y] as const;
};

const buildPath = (geometry: GeoJsonGeometry, width: number, height: number) => {
  const polygons = geometry.type === "Polygon" ? [geometry.coordinates] : geometry.coordinates;

  return polygons
    .map((polygon) =>
      polygon
        .map((ring) => {
          const [first, ...rest] = ring;
          const [startX, startY] = project(first[0], first[1], width, height);
          const commands = [`M ${startX} ${startY}`];
          rest.forEach(([lon, lat]) => {
            const [x, y] = project(lon, lat, width, height);
            commands.push(`L ${x} ${y}`);
          });
          commands.push("Z");
          return commands.join(" ");
        })
        .join(" ")
    )
    .join(" ");
};

const MapView = ({
  geoJson,
  valuesByIso3,
  selectedIso3,
  hoveredIso3,
  hoveredCountry,
  indexLabel,
  onSelectIso3,
  onHoverIso3,
}: MapViewProps) => {
  const [tooltipPosition, setTooltipPosition] = useState({ x: 0, y: 0 });

  const values = useMemo(() => {
    return Array.from(valuesByIso3.values())
      .map((entry) => entry.value)
      .filter((value): value is number => value !== null && !Number.isNaN(value));
  }, [valuesByIso3]);

  const quantiles = useMemo(() => buildQuantiles(values), [values]);

  const minValue = quantiles[0] ?? null;
  const maxValue = quantiles[quantiles.length - 1] ?? null;

  const getColor = (value: number | null) => {
    if (value === null || Number.isNaN(value) || quantiles.length === 0) {
      return "#0a1423";
    }
    for (let i = 1; i < quantiles.length; i += 1) {
      if (value <= quantiles[i]) {
        return COLORS[i - 1];
      }
    }
    return COLORS[COLORS.length - 1];
  };

  const legendBuckets = useMemo(() => {
    if (quantiles.length === 0) {
      return [];
    }
    return COLORS.map((color, index) => ({
      color,
      range: `${formatValue(quantiles[index])} – ${formatValue(quantiles[index + 1])}`,
    }));
  }, [quantiles]);

  return (
    <section className="relative flex h-full w-full items-center justify-center bg-[radial-gradient(circle_at_top,_rgba(64,194,255,0.2),_transparent_55%),_linear-gradient(140deg,_rgba(21,31,49,0.9),_rgba(11,15,23,0.95))]">
      <div className="absolute left-6 top-6 rounded-xl border border-border bg-panel/80 px-4 py-3 text-sm text-muted backdrop-blur">
        <p className="text-xs uppercase tracking-[0.2em] text-muted">Map View</p>
        <p className="mt-1 text-text">{indexLabel}</p>
      </div>

      {geoJson ? (
        <svg
          className="h-full w-full"
          viewBox="0 0 1200 600"
          onMouseLeave={() => onHoverIso3(null)}
          onMouseMove={(event) => {
            const bounds = (event.currentTarget as SVGSVGElement).getBoundingClientRect();
            setTooltipPosition({
              x: event.clientX - bounds.left,
              y: event.clientY - bounds.top,
            });
          }}
        >
          {geoJson.features.map((feature, index) => {
            const iso3 = feature.properties.iso3 ?? "";
            const valueEntry = valuesByIso3.get(iso3);
            const value = valueEntry?.value ?? null;
            const isSelected = selectedIso3 === iso3;
            const isHovered = hoveredIso3 === iso3;

            return (
              <path
                key={iso3 || feature.properties.name || `feature-${index}`}
                d={buildPath(feature.geometry, 1200, 600)}
                fill={getColor(value)}
                stroke={isSelected ? "#7bdcff" : "#182235"}
                strokeWidth={isSelected ? 1.5 : 0.6}
                opacity={isHovered || isSelected ? 1 : 0.85}
                onMouseEnter={() => onHoverIso3(iso3)}
                onFocus={() => onHoverIso3(iso3)}
                onClick={() => onSelectIso3(iso3)}
              />
            );
          })}
        </svg>
      ) : (
        <div className="panel-card max-w-md text-center">
          <h2 className="text-xl font-semibold text-accent">MapView</h2>
          <p className="mt-3 text-sm text-muted">Loading map geometry…</p>
        </div>
      )}

      <div className="absolute bottom-6 left-6 w-64 rounded-xl border border-border bg-panel/80 p-4 text-xs text-muted backdrop-blur">
        <p className="text-[10px] uppercase tracking-[0.2em] text-muted">Legend</p>
        <p className="mt-2 text-sm text-text">{indexLabel}</p>
        <p className="mt-2 text-xs text-muted">
          Min {formatValue(minValue)} • Max {formatValue(maxValue)}
        </p>
        <div className="mt-3 space-y-2">
          {legendBuckets.map((bucket) => (
            <div key={bucket.range} className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                <span className="h-3 w-3 rounded-full" style={{ backgroundColor: bucket.color }} />
                <span>{bucket.range}</span>
              </div>
            </div>
          ))}
        </div>
      </div>

      {hoveredIso3 && hoveredCountry && (
        <div
          className="pointer-events-none absolute rounded-lg border border-border bg-surface px-3 py-2 text-xs text-text shadow-glow"
          style={{
            left: tooltipPosition.x + 12,
            top: tooltipPosition.y + 12,
          }}
        >
          <p className="font-semibold text-accent">{hoveredCountry.country}</p>
          <p className="text-muted">{formatValue(hoveredCountry.value)}</p>
        </div>
      )}
    </section>
  );
};

export default MapView;
