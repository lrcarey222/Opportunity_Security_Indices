const ScatterPlot = () => {
  return (
    <div className="panel-card">
      <p className="panel-heading">ScatterPlot</p>
      <div className="mt-4 grid grid-cols-5 gap-2">
        {Array.from({ length: 15 }).map((_, index) => (
          <div
            key={`dot-${index}`}
            className="h-3 w-3 rounded-full bg-accent"
            style={{
              opacity: 0.3 + index * 0.04,
            }}
          />
        ))}
      </div>
      <p className="mt-3 text-xs text-muted">
        Placeholder scatter plot for equity vs. resilience comparisons.
      </p>
    </div>
  );
};

export default ScatterPlot;
