import Controls from "./components/Controls";
import CountryDetails from "./components/CountryDetails";
import MapView from "./components/MapView";

const App = () => {
  return (
    <div className="flex h-full w-full bg-base text-text">
      <aside className="flex w-[280px] flex-col gap-6 border-r border-border bg-surface p-6">
        <div>
          <p className="panel-heading">NEIS Center</p>
          <h1 className="mt-2 text-2xl font-semibold text-accent">Opportunity Security</h1>
          <p className="mt-2 text-sm text-muted">
            Data experience placeholder for the Opportunity Security Indices.
          </p>
        </div>
        <Controls />
      </aside>

      <main className="relative flex flex-1 flex-col">
        <div className="absolute inset-0">
          <MapView />
        </div>
      </main>

      <aside className="flex w-[360px] flex-col gap-6 border-l border-border bg-surface p-6">
        <CountryDetails />
      </aside>
    </div>
  );
};

export default App;
