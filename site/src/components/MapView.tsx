const MapView = () => {
  return (
    <section className="flex h-full w-full items-center justify-center bg-[radial-gradient(circle_at_top,_rgba(64,194,255,0.25),_transparent_55%),_linear-gradient(140deg,_rgba(21,31,49,0.9),_rgba(11,15,23,0.95))]">
      <div className="panel-card max-w-md text-center">
        <h2 className="text-xl font-semibold text-accent">MapView</h2>
        <p className="mt-3 text-sm text-muted">
          Full-screen map placeholder. Map tiles and overlays will be connected to the mock JSON data
          later.
        </p>
      </div>
    </section>
  );
};

export default MapView;
