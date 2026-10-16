// Draw the Fence views, in nav order. The assessment travels in `a` between them.
export const FENCE_NAV: { key: string; to: string; label: string }[] = [
  { key: 'assess', to: '/assess', label: 'Assess' },
  { key: 'result', to: '/result', label: 'Result' },
  { key: 'scenario', to: '/scenario', label: 'Test another world' },
  { key: 'fence', to: '/fence', label: 'Build your fence' },
  { key: 'supply', to: '/supply', label: 'Trusted supply' },
  { key: 'examples', to: '/examples', label: 'Examples' },
  { key: 'methodology', to: '/methodology', label: 'Methodology' },
];
