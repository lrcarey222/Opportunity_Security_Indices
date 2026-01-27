const StackedBar = () => {
  return (
    <div className="panel-card">
      <p className="panel-heading">StackedBar</p>
      <div className="mt-4 space-y-3 text-sm text-muted">
        <div className="h-3 w-full overflow-hidden rounded-full bg-base">
          <div className="flex h-full">
            <div className="h-full w-[35%] bg-accent"></div>
            <div className="h-full w-[25%] bg-accent-strong"></div>
            <div className="h-full w-[40%] bg-border"></div>
          </div>
        </div>
        <div className="flex justify-between">
          <span>Inclusion</span>
          <span className="text-text">35%</span>
        </div>
        <div className="flex justify-between">
          <span>Resilience</span>
          <span className="text-text">25%</span>
        </div>
        <div className="flex justify-between">
          <span>Safety</span>
          <span className="text-text">40%</span>
        </div>
      </div>
    </div>
  );
};

export default StackedBar;
