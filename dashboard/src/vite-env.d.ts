/// <reference types="vite/client" />

declare module 'world-atlas/countries-50m.json' {
  const topology: unknown;
  export default topology;
}

declare module '*.md?raw' {
  const text: string;
  export default text;
}
