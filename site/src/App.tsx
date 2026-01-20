import { useEffect, useMemo, useState } from "react";
import Controls from "./components/Controls";
import CountryDetails from "./components/CountryDetails";
import MapView from "./components/MapView";
import { ensureNumber, parseCompactTable } from "./utils/data";

type IndexRow = {
  iso3: string;
  country: string;
  tech: string;
  supply_chain: string;
  sub_sector?: string;
  energy_security_index: number | null;
  economic_opportunity_index: number | null;
};

type LatestYearPayload = {
  year: number | null;
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

type Pillar = "Energy Security" | "Economic Opportunity";

const PILLARS: Pillar[] = ["Energy Security", "Economic Opportunity"];

const App = () => {
  const [indices, setIndices] = useState<IndexRow[]>([]);
  const [latestYear, setLatestYear] = useState<number | null>(null);
  const [geoJson, setGeoJson] = useState<GeoJsonFeatureCollection | null>(null);

  const [pillar, setPillar] = useState<Pillar>("Energy Security");
  const [selectedTech, setSelectedTech] = useState<string>("");
  const [selectedSupplyChain, setSelectedSupplyChain] = useState<string>("");
  const [selectedYear, setSelectedYear] = useState<number | null>(null);
  const [selectedIso3, setSelectedIso3] = useState<string | null>(null);
  const [hoveredIso3, setHoveredIso3] = useState<string | null>(null);

  useEffect(() => {
    const loadData = async () => {
      const [indicesResponse, latestYearResponse, geoJsonResponse] = await Promise.all([
        fetch("/data/indices.json"),
        fetch("/data/latest_year.json"),
        fetch("/data/countries.geojson"),
      ]);

      const indicesPayload = await indicesResponse.json();
      const latestYearPayload = (await latestYearResponse.json()) as LatestYearPayload;
      const geoJsonPayload = (await geoJsonResponse.json()) as GeoJsonFeatureCollection;

      const parsedIndices = parseCompactTable<IndexRow>(indicesPayload).map((row) => ({
        ...row,
        energy_security_index: ensureNumber(row.energy_security_index),
        economic_opportunity_index: ensureNumber(row.economic_opportunity_index),
      }));

      setIndices(parsedIndices);
      setLatestYear(latestYearPayload.year ?? null);
      setGeoJson(geoJsonPayload);
    };

    void loadData();
  }, []);

  const techOptions = useMemo(() => {
    return Array.from(new Set(indices.map((row) => row.tech))).sort();
  }, [indices]);

  const supplyChainOptions = useMemo(() => {
    return Array.from(new Set(indices.map((row) => row.supply_chain))).sort();
  }, [indices]);

  useEffect(() => {
    if (techOptions.length > 0 && !selectedTech) {
      setSelectedTech(techOptions[0]);
    }
  }, [techOptions, selectedTech]);

  useEffect(() => {
    if (supplyChainOptions.length > 0 && !selectedSupplyChain) {
      const midstream = supplyChainOptions.find((option) => option.toLowerCase() === "midstream");
      setSelectedSupplyChain(midstream ?? supplyChainOptions[0]);
    }
  }, [supplyChainOptions, selectedSupplyChain]);

  useEffect(() => {
    if (latestYear !== null && selectedYear === null) {
      setSelectedYear(latestYear);
    }
  }, [latestYear, selectedYear]);

  const filteredIndices = useMemo(() => {
    return indices.filter((row) => {
      if (selectedTech && row.tech !== selectedTech) {
        return false;
      }
      if (selectedSupplyChain && row.supply_chain !== selectedSupplyChain) {
        return false;
      }
      return true;
    });
  }, [indices, selectedTech, selectedSupplyChain]);

  const selectedIndexKey = pillar === "Energy Security" ? "energy_security_index" : "economic_opportunity_index";

  const valuesByIso3 = useMemo(() => {
    const map = new Map<string, { value: number | null; country: string }>();
    filteredIndices.forEach((row) => {
      map.set(row.iso3, {
        value: row[selectedIndexKey],
        country: row.country,
      });
    });
    return map;
  }, [filteredIndices, selectedIndexKey]);

  const selectedCountry = useMemo(() => {
    if (!selectedIso3) {
      return null;
    }
    return filteredIndices.find((row) => row.iso3 === selectedIso3) ?? null;
  }, [filteredIndices, selectedIso3]);

  const hoveredCountry = useMemo(() => {
    if (!hoveredIso3) {
      return null;
    }
    return valuesByIso3.get(hoveredIso3) ?? null;
  }, [hoveredIso3, valuesByIso3]);

  useEffect(() => {
    if (selectedIso3 && !valuesByIso3.has(selectedIso3)) {
      setSelectedIso3(null);
    }
  }, [selectedIso3, valuesByIso3]);

  return (
    <div className="flex h-full w-full bg-base text-text">
      <aside className="flex w-[280px] flex-col gap-6 border-r border-border bg-surface p-6">
        <div>
          <p className="panel-heading">NEIS Center</p>
          <h1 className="mt-2 text-2xl font-semibold text-accent">Opportunity Security</h1>
          <p className="mt-2 text-sm text-muted">
            Interactive explorer for Opportunity Security indices and component data.
          </p>
        </div>
        <Controls
          pillars={PILLARS}
          pillar={pillar}
          onPillarChange={setPillar}
          techOptions={techOptions}
          selectedTech={selectedTech}
          onTechChange={setSelectedTech}
          supplyChainOptions={supplyChainOptions}
          selectedSupplyChain={selectedSupplyChain}
          onSupplyChainChange={setSelectedSupplyChain}
          years={latestYear ? [latestYear] : []}
          selectedYear={selectedYear}
          onYearChange={(year) => setSelectedYear(year)}
        />
      </aside>

      <main className="relative flex flex-1 flex-col">
        <div className="absolute inset-0">
          <MapView
            geoJson={geoJson}
            valuesByIso3={valuesByIso3}
            selectedIso3={selectedIso3}
            hoveredIso3={hoveredIso3}
            hoveredCountry={hoveredCountry}
            indexLabel={pillar}
            onSelectIso3={setSelectedIso3}
            onHoverIso3={setHoveredIso3}
          />
        </div>
      </main>

      <aside className="flex w-[360px] flex-col gap-6 border-l border-border bg-surface p-6">
        <CountryDetails
          selectedCountry={selectedCountry}
          pillar={pillar}
          selectedYear={selectedYear}
          selectedTech={selectedTech}
          selectedSupplyChain={selectedSupplyChain}
        />
      </aside>
    </div>
  );
};

export default App;
