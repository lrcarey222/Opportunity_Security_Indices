import ScatterPlot from "./ScatterPlot";
import StackedBar from "./StackedBar";

type CountryDetailsProps = {
  selectedCountry: {
    iso3: string;
    country: string;
    energy_security_index: number | null;
    economic_opportunity_index: number | null;
  } | null;
  pillar: string;
  selectedYear: number | null;
  selectedTech: string;
  selectedSupplyChain: string;
};

const formatValue = (value: number | null) => {
  if (value === null || Number.isNaN(value)) {
    return "–";
  }
  return value.toFixed(1);
};

const CountryDetails = ({
  selectedCountry,
  pillar,
  selectedYear,
  selectedTech,
  selectedSupplyChain,
}: CountryDetailsProps) => {
  const energyValue = formatValue(selectedCountry?.energy_security_index ?? null);
  const opportunityValue = formatValue(selectedCountry?.economic_opportunity_index ?? null);

  return (
    <section className="flex h-full flex-col gap-6">
      <div>
        <p className="panel-heading">Details</p>
        <h2 className="mt-2 text-2xl font-semibold text-text">
          {selectedCountry?.country ?? "Select a country"}
        </h2>
        <p className="mt-2 text-sm text-muted">
          {selectedCountry
            ? `${selectedTech} • ${selectedSupplyChain} • ${selectedYear ?? "latest"}`
            : "Click a country on the map to inspect its index profile."}
        </p>
      </div>

      <div className="panel-card">
        <p className="panel-heading">Selected pillar</p>
        <div className="mt-4 flex items-baseline justify-between">
          <span className="text-4xl font-semibold text-accent">
            {pillar === "Energy Security" ? energyValue : opportunityValue}
          </span>
          <span className="rounded-full border border-border px-3 py-1 text-xs text-muted">
            {pillar}
          </span>
        </div>
        <div className="mt-4 grid grid-cols-2 gap-3 text-sm text-muted">
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Energy Security</p>
            <p className="mt-1 text-text">{energyValue}</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Opportunity</p>
            <p className="mt-1 text-text">{opportunityValue}</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">ISO3</p>
            <p className="mt-1 text-text">{selectedCountry?.iso3 ?? "-"}</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Year</p>
            <p className="mt-1 text-text">{selectedYear ?? "-"}</p>
          </div>
        </div>
      </div>

      <StackedBar />
      <ScatterPlot />
    </section>
  );
};

export default CountryDetails;
