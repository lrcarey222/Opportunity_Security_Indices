// Draw the Fence: one-click demonstrations. Each example selects a curated component or system and
// sets a few assumptions of its own (shown as YOUR JUDGMENT, all editable). They demonstrate that the
// framework gives different answers for different problems; they do not settle any of them.

import type { AssessState, LayerState } from '../state/assessment.ts';

export interface Example {
  id: string;
  title: string;
  question: string;
  summary: string;
  /** what the example is meant to show (not a forced result: the engine decides) */
  shows: string;
  state: () => AssessState;
  view: 'result' | 'fence' | 'diversify' | 'scenario';
}

const layer = (l: Partial<LayerState>): LayerState => ({ answers: {}, scenario: {}, strategies: [], ...l });

const base = (sel: AssessState['sel'], layers: AssessState['layers'], focus?: string, example?: string): AssessState => ({
  country: 'USA',
  sel,
  focus,
  mode: 'analyst',
  layers,
  step: 6,
  example,
});

export const EXAMPLES: Example[] = [
  {
    id: 'grid-control',
    title: 'Grid-control software',
    question: 'SCADA, energy-management and DER-management systems that operate the grid',
    summary: 'A digitally active control layer with severe, immediate consequences that can be isolated and sourced from trusted vendors.',
    shows: 'A narrow, high fence around the control layer only.',
    view: 'result',
    state: () =>
      base({ tech: 'Electric Grid', stage: 'Midstream', component: 'grid-scada' }, {
        'grid-scada': layer({ policy: { levels: { trustedSource: 3, cyberStandard: 3 }, scope: 'control-layer' } }),
      }, 'grid-scada', 'grid-control'),
  },
  {
    id: 'magnets',
    title: 'Rare-earth permanent magnets',
    question: 'Sintered NdFeB magnets for motors, generators and defence systems',
    summary: 'A physical chokepoint: refining is concentrated in one non-coalition supplier, substitutes are poor and new capacity takes years.',
    shows: 'A small yard for a physical input, with remediation through trusted diversification rather than self-sufficiency.',
    view: 'result',
    state: () => base({ tech: 'Wind', stage: 'Midstream', component: 'ree-magnets' }, {}, 'ree-magnets', 'magnets'),
  },
  {
    id: 'battery-cells',
    title: 'Battery cells',
    question: 'LFP and NMC lithium-ion cells for storage and vehicles',
    summary: 'No acute control or chokepoint problem, but production carries process learning that bears on successor chemistries.',
    shows: 'A medium yard: compete while remaining selectively open. The example tests Preserve + Leap ahead.',
    view: 'result',
    state: () =>
      base({ tech: 'Batteries', stage: 'Midstream', component: 'bess-cells' }, {
        'bess-cells': layer({
          answers: { premium: 15, costPerfAdvantage: 'yes', incumbentVulnerable: 'partly', scalePathway: 'partly', beyondNovelty: 'partly' },
          strategies: ['preserve', 'leapAhead'],
          why: 'learning',
        }),
      }, 'bess-cells', 'battery-cells'),
  },
  {
    id: 'solar-modules',
    title: 'Conventional solar modules',
    question: 'Mature, passive, globally traded crystalline-silicon modules',
    summary: 'The paper’s hard case. With a high domestic premium the capability case does not justify protection; with a low one it might.',
    shows: 'A large yard, or a large yard with a limited capability floor, depending on the premium. Try the slider.',
    view: 'scenario',
    state: () =>
      base({ tech: 'Solar', stage: 'Midstream', component: 'pv-modules' }, {
        'pv-modules': layer({ answers: { premium: 40 }, why: 'learning', policy: { levels: { tariff: 3, productionSubsidy: 2 }, scope: 'whole-product' } }),
      }, 'pv-modules', 'solar-modules'),
  },
  {
    id: 'battery-storage',
    title: 'Utility-scale battery storage',
    question: 'One system, seven layers',
    summary: 'Control software, cells, materials, power electronics and enclosures face different tests.',
    shows: 'A mixed result: different yards for different layers.',
    view: 'result',
    state: () =>
      base({ tech: 'Batteries', stage: 'Midstream', stack: 'battery-storage' }, {
        'bess-cells': layer({ answers: { premium: 15 }, strategies: ['preserve'] }),
        'bess-cathode': layer({ answers: { premium: 15 } }),
        'bess-anode': layer({ answers: { premium: 15 } }),
        'bess-pcs': layer({ answers: { premium: 15 } }),
      }, 'bess-control', 'battery-storage'),
  },
  {
    id: 'nuclear-fuel',
    title: 'Nuclear fuel and controls',
    question: 'Reactor controls, HALEU and LEU enrichment',
    summary: 'Three parts of one fuel cycle, three different tests; LEU is left with its evidence gaps open.',
    shows: 'Precision inside a sector, and what an indeterminate result looks like.',
    view: 'result',
    state: () => base({ tech: 'Nuclear', stage: 'Midstream', stack: 'nuclear-fuel' }, {}, 'nuc-haleu', 'nuclear-fuel'),
  },
];

export const exampleById = (id: string | undefined) => EXAMPLES.find((e) => e.id === id);
