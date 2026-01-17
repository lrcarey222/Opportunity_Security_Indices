const Controls = () => {
  return (
    <section className="flex flex-1 flex-col gap-6">
      <div className="panel-card">
        <p className="panel-heading">Controls</p>
        <div className="mt-4 flex flex-col gap-3 text-sm text-muted">
          <div className="flex items-center justify-between rounded-lg border border-border px-3 py-2">
            <span>Year</span>
            <span className="text-text">2024</span>
          </div>
          <div className="flex items-center justify-between rounded-lg border border-border px-3 py-2">
            <span>Index</span>
            <span className="text-text">Opportunity</span>
          </div>
          <div className="flex items-center justify-between rounded-lg border border-border px-3 py-2">
            <span>Region</span>
            <span className="text-text">Global</span>
          </div>
        </div>
      </div>

      <div className="panel-card">
        <p className="panel-heading">Highlights</p>
        <ul className="mt-4 space-y-2 text-sm text-muted">
          <li className="flex items-center justify-between">
            <span>Top score</span>
            <span className="text-accent">92.4</span>
          </li>
          <li className="flex items-center justify-between">
            <span>Median</span>
            <span className="text-text">68.2</span>
          </li>
          <li className="flex items-center justify-between">
            <span>Coverage</span>
            <span className="text-text">180 countries</span>
          </li>
        </ul>
      </div>
    </section>
  );
};

export default Controls;
