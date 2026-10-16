// Draw the Fence: the policy instruments consistent with each diagnosis, and those that do not
// follow from it. Text only; the sandbox rules live in engine/policy.ts.

import type { YardVariant } from '../engine/index.ts';

export interface Toolkit {
  recommended: string[];
  avoid: string[];
}

const SMALL_DIGITAL: Toolkit = {
  recommended: [
    'Trusted-source requirements for firmware, communications and remote-access functions',
    'Cybersecurity certification, testing and continuous monitoring',
    'Restrictions on remote access and over-the-air updates by untrusted vendors',
    'A replacement programme for the installed base, with deadlines',
    'R&D and standards for separable, certifiable control architectures',
  ],
  avoid: [
    'Tariffs or bans on the whole hardware stack when the risk sits in one layer',
    'Treating passive components as though they carried the control risk',
    'Counting a rule or an announcement as a remediated vulnerability',
  ],
};

export const TOOLKITS: Record<YardVariant | 'indeterminate', Toolkit> = {
  'small-control': SMALL_DIGITAL,
  'small-integrated': {
    recommended: [...SMALL_DIGITAL.recommended.slice(0, 4), 'Support for redesigns that separate the control layer, so the fence can shrink'],
    avoid: SMALL_DIGITAL.avoid.slice(1),
  },
  'small-whole': {
    recommended: ['Trusted sourcing for the whole product while the control function cannot be separated', ...SMALL_DIGITAL.recommended.slice(1, 4), 'A plan and a date for separating the control layer'],
    avoid: ['Permanent whole-product restrictions once separation becomes possible', 'Counting a rule or an announcement as a remediated vulnerability'],
  },
  'small-unclear': {
    recommended: ['Establish how separable the control function is (testing, vendor disclosure)', ...SMALL_DIGITAL.recommended.slice(0, 3)],
    avoid: SMALL_DIGITAL.avoid,
  },
  'small-both': {
    recommended: [
      'Trusted-source and cybersecurity requirements for the control layer',
      'Diversified, qualified supply from trusted producers',
      'Stockpiles sized to bridge the time to restore supply',
      'A replacement programme for the installed base',
      'Stress tests of a prolonged cutoff and of a compromised control layer',
    ],
    avoid: ['Treating either problem as solved because the other is addressed', 'Counting announcements as remediation'],
  },
  'small-input': {
    recommended: [
      'Diversified, qualified supply from trusted producers (offtake, public finance, allied agreements)',
      'Stockpiles sized to bridge the time to restore supply',
      'Surge capacity and recycling',
      'Liquidity support for vulnerable sub-tier suppliers',
      'Stress tests of a prolonged cutoff, repeated until it can be survived',
    ],
    avoid: [
      'Declaring the chokepoint solved when capacity is announced rather than qualified',
      'Self-sufficiency as the goal when trusted diversification would remove the leverage',
      'R&D support alone: it does not keep critical production running through a cutoff',
    ],
  },
  medium: {
    recommended: [
      'Time-limited production incentives tied to capability milestones',
      'R&D, demonstration and first-of-a-kind finance',
      'Workforce and supplier development',
      'Performance-based public procurement',
      'Selective foreign licensing or joint ventures, on terms that transfer capability',
      'Targeted trade measures where unfair competition threatens the learning base',
      'Performance requirements and sunset reviews',
    ],
    avoid: [
      'Blanket security bans',
      'Total exclusion of foreign technology when learning from the leader would speed catch-up',
      'Permanent protection without competitiveness metrics',
    ],
  },
  'medium-costly': {
    recommended: [
      'Capability-building support (R&D, finance, workforce, suppliers) over measures that raise buyers’ prices',
      'Selective foreign licensing or joint ventures, on terms that transfer capability',
      'Performance requirements, a stated premium and a review date',
    ],
    avoid: ['High tariffs or content rules whose cost falls on deployment', 'Blanket security bans', 'Permanent protection without competitiveness metrics'],
  },
  large: {
    recommended: [
      'Open, competitive sourcing from diversified suppliers',
      'Trade agreements and standards that widen the supplier base',
      'Cross-cutting safeguards applied in every yard: forced-labour, safety, environmental rules and legitimate trade remedies',
    ],
    avoid: [
      'Tariffs or domestic-content rules without a stated strategic benefit',
      'Production subsidies justified by jobs alone, presented as security',
      'Security restrictions on a good with no control layer or chokepoint',
    ],
  },
  'large-diversify': {
    recommended: [
      'Diversification toward trusted producers, not a domestic replica',
      'Transparency and supply-chain mapping requirements',
      'Procurement tools that reward supplier diversity',
      'Cross-cutting safeguards applied in every yard',
    ],
    avoid: ['High tariffs that raise deployment costs', 'Full domestic self-sufficiency as the goal', 'Security restrictions without a security diagnosis'],
  },
  'large-floor': {
    recommended: [
      'A limited domestic floor with a stated purpose: what capability it preserves and how much is enough',
      'An explicit premium buyers are asked to pay, and a review date',
      'Diversified trusted supply for the rest of demand',
      'Separate support for next-generation designs, assessed on their own',
    ],
    avoid: ['A floor that quietly becomes permanent protection', 'Raising the cost of the whole market to protect the floor', 'Security restrictions without a security diagnosis'],
  },
  indeterminate: {
    recommended: ['Measures that buy information: supply mapping, testing, vendor disclosure', 'Small, reversible pilots', 'A date to reassess once the pivotal gaps are filled'],
    avoid: ['Irreversible protection or bans before the diagnosis is settled'],
  },
};
