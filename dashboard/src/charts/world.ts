// World geometry for the maps: world-atlas 50 m TopoJSON (ISO 3166 numeric ids, joined on
// dims.countries.iso_n3), loaded once on the first map (a separate chunk), Antarctica dropped.

import type { Feature, FeatureCollection, Geometry } from 'geojson';

export type CountryFeature = Feature<Geometry, { name?: string }> & { id?: string | number };

export interface World {
  countries: CountryFeature[];
  /** every country except Antarctica, for fitting the projection */
  fit: FeatureCollection;
}

let worldP: Promise<World> | null = null;

export function loadWorld(): Promise<World> {
  if (!worldP) {
    worldP = Promise.all([import('world-atlas/countries-50m.json'), import('topojson-client')]).then(([topoMod, topo]) => {
      const topology = (topoMod as { default?: unknown }).default ?? topoMod;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const fc = topo.feature(topology as any, (topology as any).objects.countries) as unknown as FeatureCollection<Geometry, { name?: string }>;
      const countries = (fc.features as CountryFeature[]).filter((f) => String(f.id) !== '010');
      return { countries, fit: { type: 'FeatureCollection', features: countries } as FeatureCollection };
    });
    worldP.catch(() => {
      worldP = null;
    });
  }
  return worldP;
}
