import ScatterPlot from "./ScatterPlot";
import StackedBar from "./StackedBar";

const CountryDetails = () => {
  return (
    <section className="flex h-full flex-col gap-6">
      <div>
        <p className="panel-heading">Details</p>
        <h2 className="mt-2 text-2xl font-semibold text-text">Country Snapshot</h2>
        <p className="mt-2 text-sm text-muted">
          Placeholder panel for country-level indicators and data-driven insights.
        </p>
      </div>

      <div className="panel-card">
        <p className="panel-heading">Opportunity Index</p>
        <div className="mt-4 flex items-baseline justify-between">
          <span className="text-4xl font-semibold text-accent">78.3</span>
          <span className="rounded-full border border-border px-3 py-1 text-xs text-muted">
            +2.1 YoY
          </span>
        </div>
        <div className="mt-4 grid grid-cols-2 gap-3 text-sm text-muted">
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Civic space</p>
            <p className="mt-1 text-text">82</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Economic access</p>
            <p className="mt-1 text-text">74</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Security</p>
            <p className="mt-1 text-text">69</p>
          </div>
          <div className="rounded-lg border border-border px-3 py-2">
            <p className="text-xs uppercase tracking-wide">Governance</p>
            <p className="mt-1 text-text">88</p>
          </div>
        </div>
      </div>

      <StackedBar />
      <ScatterPlot />
    </section>
  );
};

export default CountryDetails;
