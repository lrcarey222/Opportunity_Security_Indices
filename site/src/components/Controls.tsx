type ControlsProps = {
  pillars: string[];
  pillar: string;
  onPillarChange: (pillar: string) => void;
  techOptions: string[];
  selectedTech: string;
  onTechChange: (value: string) => void;
  supplyChainOptions: string[];
  selectedSupplyChain: string;
  onSupplyChainChange: (value: string) => void;
  years: number[];
  selectedYear: number | null;
  onYearChange: (value: number | null) => void;
};

const Controls = ({
  pillars,
  pillar,
  onPillarChange,
  techOptions,
  selectedTech,
  onTechChange,
  supplyChainOptions,
  selectedSupplyChain,
  onSupplyChainChange,
  years,
  selectedYear,
  onYearChange,
}: ControlsProps) => {
  return (
    <section className="flex flex-1 flex-col gap-6">
      <div className="panel-card">
        <p className="panel-heading">Controls</p>
        <div className="mt-4 flex flex-col gap-4 text-sm text-muted">
          <label className="flex flex-col gap-2">
            <span className="text-xs uppercase tracking-wide">Pillar</span>
            <select
              className="rounded-lg border border-border bg-base px-3 py-2 text-text"
              value={pillar}
              onChange={(event) => onPillarChange(event.target.value)}
            >
              {pillars.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>

          <label className="flex flex-col gap-2">
            <span className="text-xs uppercase tracking-wide">Technology</span>
            <select
              className="rounded-lg border border-border bg-base px-3 py-2 text-text"
              value={selectedTech}
              onChange={(event) => onTechChange(event.target.value)}
            >
              {techOptions.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>

          <label className="flex flex-col gap-2">
            <span className="text-xs uppercase tracking-wide">Supply chain</span>
            <select
              className="rounded-lg border border-border bg-base px-3 py-2 text-text"
              value={selectedSupplyChain}
              onChange={(event) => onSupplyChainChange(event.target.value)}
            >
              {supplyChainOptions.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>

          <label className="flex flex-col gap-2">
            <span className="text-xs uppercase tracking-wide">Year</span>
            <select
              className="rounded-lg border border-border bg-base px-3 py-2 text-text"
              value={selectedYear ?? ""}
              onChange={(event) => {
                const value = event.target.value ? Number(event.target.value) : null;
                onYearChange(value);
              }}
            >
              {years.map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </label>
        </div>
      </div>

      <div className="panel-card">
        <p className="panel-heading">Highlights</p>
        <ul className="mt-4 space-y-2 text-sm text-muted">
          <li className="flex items-center justify-between">
            <span>Selected pillar</span>
            <span className="text-text">{pillar}</span>
          </li>
          <li className="flex items-center justify-between">
            <span>Supply chain</span>
            <span className="text-text">{selectedSupplyChain || "-"}</span>
          </li>
          <li className="flex items-center justify-between">
            <span>Year</span>
            <span className="text-text">{selectedYear ?? "-"}</span>
          </li>
        </ul>
      </div>
    </section>
  );
};

export default Controls;
