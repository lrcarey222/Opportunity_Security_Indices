// Yards & Fences decision engine: the policy sandbox ("Build your fence").
//
// A package is a set of instruments at intensity 0-3 plus the scope its security measures apply to.
// evaluatePolicyFit() compares the package with the diagnosis using explicit rules and returns
// findings (under-fencing, over-fencing, mismatch, closed gate, missing exit, ...), the purposes the
// package covers against those the diagnosis needs, and a simple fence geometry for the diagram.

import type { Strategy, YardOutcome, YardResult, YardVariant } from './types.ts';

export type PolicyId =
  | 'trustedSource'
  | 'cyberStandard'
  | 'tariff'
  | 'productionSubsidy'
  | 'procurement'
  | 'stockpile'
  | 'exportControls'
  | 'investmentRestriction'
  | 'rnd'
  | 'publicFinance'
  | 'licensingJV'
  | 'workforce'
  | 'performanceSunset';

export type Intensity = 0 | 1 | 2 | 3;
export type Purpose = 'secure' | 'buffer' | 'build' | 'protect' | 'open' | 'discipline';

export interface PolicyDef {
  id: PolicyId;
  label: string;
  purpose: Purpose;
  /** whole-product instruments restrict or favour the entire product, not one layer */
  breadth: 'targeted' | 'whole-product' | 'scoped';
  /** direct effect on buyers' prices per intensity step (0 none, 1 some, 2 strong) */
  buyerCost: 0 | 1 | 2;
  description: string;
}

export const POLICIES: PolicyDef[] = [
  { id: 'trustedSource', label: 'Trusted-source requirement', purpose: 'secure', breadth: 'scoped', buyerCost: 1, description: 'Only vetted suppliers may provide the covered layer or product.' },
  { id: 'cyberStandard', label: 'Cybersecurity standard', purpose: 'secure', breadth: 'targeted', buyerCost: 0, description: 'Certification, testing and monitoring requirements for software, firmware and remote access.' },
  { id: 'tariff', label: 'Tariff', purpose: 'protect', breadth: 'whole-product', buyerCost: 2, description: 'A tax on imports of the product.' },
  { id: 'productionSubsidy', label: 'Domestic production subsidy', purpose: 'build', breadth: 'whole-product', buyerCost: 0, description: 'Production tax credits or grants for domestic output.' },
  { id: 'procurement', label: 'Procurement preference', purpose: 'protect', breadth: 'whole-product', buyerCost: 1, description: 'Public buyers favour domestic or trusted products.' },
  { id: 'stockpile', label: 'Stockpile', purpose: 'buffer', breadth: 'targeted', buyerCost: 0, description: 'Strategic inventories that bridge a supply cutoff.' },
  { id: 'exportControls', label: 'Export controls', purpose: 'secure', breadth: 'whole-product', buyerCost: 0, description: 'Limits on selling technology or equipment to rivals.' },
  { id: 'investmentRestriction', label: 'Investment restriction', purpose: 'secure', breadth: 'whole-product', buyerCost: 1, description: 'Screening or prohibition of foreign ownership and inbound investment.' },
  { id: 'rnd', label: 'R&D and demonstration', purpose: 'build', breadth: 'targeted', buyerCost: 0, description: 'Research, pilots and first-of-a-kind demonstration.' },
  { id: 'publicFinance', label: 'Public finance', purpose: 'build', breadth: 'targeted', buyerCost: 0, description: 'Loans, guarantees and equity for plants and suppliers.' },
  { id: 'licensingJV', label: 'Foreign licensing / JV access', purpose: 'open', breadth: 'targeted', buyerCost: 0, description: 'A gate in the fence: admit foreign technology on terms that transfer capability.' },
  { id: 'workforce', label: 'Workforce and supplier development', purpose: 'build', breadth: 'targeted', buyerCost: 0, description: 'Training, supplier qualification and engineering capability.' },
  { id: 'performanceSunset', label: 'Performance requirements and sunset review', purpose: 'discipline', breadth: 'targeted', buyerCost: 0, description: 'Milestones, competitiveness metrics and a date to reassess support.' },
];

export const POLICY_BY_ID = Object.fromEntries(POLICIES.map((p) => [p.id, p])) as Record<PolicyId, PolicyDef>;

export interface PolicyPackage {
  levels: Partial<Record<PolicyId, Intensity>>;
  /** What the security instruments (trusted source, standards) apply to. */
  securityScope: 'control-layer' | 'whole-product';
}

export type FindingKind =
  | 'under-fencing'
  | 'over-fencing'
  | 'mismatch'
  | 'closed-gate'
  | 'no-exit'
  | 'remediation-gap'
  | 'misapplied'
  | 'leap-unfunded'
  | 'cost'
  | 'aligned'
  | 'empty';

export interface PolicyFinding {
  kind: FindingKind;
  severity: 'warning' | 'caution' | 'info' | 'ok';
  title: string;
  text: string;
  policies: PolicyId[];
}

export type Need = 'required' | 'helpful' | 'neutral' | 'avoid';

export interface FenceShape {
  /** 0 (none) to 3 (high) */
  height: number;
  /** how much of the product the fence surrounds */
  breadth: 'narrow' | 'wide';
  /** permeability: 0 closed to 3 open */
  gates: number;
}

export interface PolicyFit {
  findings: PolicyFinding[];
  coverage: Record<Purpose, number>;
  needs: Record<Purpose, Need>;
  costPressure: 'none' | 'low' | 'moderate' | 'high';
  chosen: FenceShape;
  diagnosed: FenceShape;
}

const lv = (p: PolicyPackage, id: PolicyId): number => p.levels[id] ?? 0;

const PURPOSE_TEXT: Record<Purpose, string> = {
  secure: 'secure the vulnerable layer',
  buffer: 'bridge a cutoff',
  build: 'build capability',
  protect: 'protect the product from imports',
  open: 'keep gates open to foreign technology',
  discipline: 'set performance requirements and an exit',
};
const sum = (p: PolicyPackage, ids: PolicyId[]) => ids.reduce((s, id) => s + lv(p, id), 0);

export function needsFor(variant: YardVariant | 'indeterminate'): Record<Purpose, Need> {
  switch (variant) {
    case 'small-control':
    case 'small-integrated':
    case 'small-whole':
    case 'small-unclear':
      return { secure: 'required', buffer: 'helpful', build: 'helpful', protect: 'avoid', open: 'neutral', discipline: 'helpful' };
    case 'small-input':
      return { secure: 'helpful', buffer: 'required', build: 'required', protect: 'neutral', open: 'neutral', discipline: 'helpful' };
    case 'small-both':
      return { secure: 'required', buffer: 'required', build: 'required', protect: 'neutral', open: 'neutral', discipline: 'helpful' };
    case 'medium':
    case 'medium-costly':
      return { secure: 'avoid', buffer: 'neutral', build: 'required', protect: variant === 'medium-costly' ? 'avoid' : 'neutral', open: 'helpful', discipline: 'required' };
    case 'large-floor':
      return { secure: 'avoid', buffer: 'neutral', build: 'helpful', protect: 'avoid', open: 'helpful', discipline: 'required' };
    case 'large-diversify':
      return { secure: 'avoid', buffer: 'helpful', build: 'neutral', protect: 'avoid', open: 'helpful', discipline: 'neutral' };
    case 'large':
      return { secure: 'avoid', buffer: 'neutral', build: 'neutral', protect: 'avoid', open: 'helpful', discipline: 'neutral' };
    case 'indeterminate':
      return { secure: 'neutral', buffer: 'helpful', build: 'helpful', protect: 'avoid', open: 'neutral', discipline: 'required' };
  }
}

export function diagnosedFence(variant: YardVariant | 'indeterminate'): FenceShape {
  if (variant === 'small-control') return { height: 3, breadth: 'narrow', gates: 0 };
  if (variant === 'small-integrated' || variant === 'small-unclear') return { height: 3, breadth: 'narrow', gates: 0 };
  if (variant === 'small-whole') return { height: 3, breadth: 'wide', gates: 0 };
  if (variant === 'small-input') return { height: 2, breadth: 'narrow', gates: 1 };
  if (variant === 'small-both') return { height: 3, breadth: 'narrow', gates: 0 };
  if (variant === 'medium' || variant === 'medium-costly') return { height: 2, breadth: 'wide', gates: 2 };
  if (variant === 'large-floor') return { height: 1, breadth: 'narrow', gates: 3 };
  if (variant === 'indeterminate') return { height: 1, breadth: 'narrow', gates: 2 };
  return { height: 0, breadth: 'wide', gates: 3 };
}

const RESTRICTIVE: PolicyId[] = ['trustedSource', 'cyberStandard', 'tariff', 'procurement', 'exportControls', 'investmentRestriction'];
const BUILD: PolicyId[] = ['productionSubsidy', 'rnd', 'publicFinance', 'workforce'];
const PROTECT: PolicyId[] = ['tariff', 'procurement'];

export function evaluatePolicyFit(result: Pick<YardResult, 'yard' | 'variant' | 'strategies'>, pkg: PolicyPackage): PolicyFit {
  const yard: YardOutcome = result.yard;
  const variant: YardVariant | 'indeterminate' = yard === 'indeterminate' || yard === 'mixed' ? 'indeterminate' : result.variant;
  const strategies = new Set<Strategy>(result.strategies);
  const findings: PolicyFinding[] = [];
  const add = (f: PolicyFinding) => findings.push(f);
  const chosenIds = (Object.keys(pkg.levels) as PolicyId[]).filter((id) => lv(pkg, id) > 0);

  const coverage = { secure: 0, buffer: 0, build: 0, protect: 0, open: 0, discipline: 0 } as Record<Purpose, number>;
  for (const p of POLICIES) coverage[p.purpose] += lv(pkg, p.id);

  const secureN = sum(pkg, ['trustedSource', 'cyberStandard']);
  const wideSecurity = pkg.securityScope === 'whole-product' && lv(pkg, 'trustedSource') > 0;
  const hardwareRestriction = lv(pkg, 'tariff') >= 2 || lv(pkg, 'investmentRestriction') >= 3 || lv(pkg, 'procurement') >= 3;
  const buildN = sum(pkg, BUILD);
  const protectN = sum(pkg, PROTECT);

  if (chosenIds.length === 0) {
    add({ kind: 'empty', severity: 'info', title: 'No instruments chosen', text: 'Choose instruments and intensities to compare your fence with the diagnosis.', policies: [] });
  } else {
    const isSmallDigital = variant === 'small-control' || variant === 'small-integrated' || variant === 'small-whole' || variant === 'small-unclear' || variant === 'small-both';
    const isSmallInput = variant === 'small-input' || variant === 'small-both';
    if (isSmallDigital && secureN < 2)
      add({
        kind: 'under-fencing',
        severity: 'warning',
        title: 'Under-fencing risk',
        text: 'You identified an acute control-layer vulnerability, but the package has little trusted-source or cybersecurity protection.' +
          (coverage.build > 0 && secureN === 0 ? ' General support such as R&D or subsidies does not close a command path.' : ''),
        policies: ['trustedSource', 'cyberStandard'],
      });
    if (isSmallInput && coverage.buffer + sum(pkg, ['publicFinance', 'productionSubsidy']) < 3)
      add({
        kind: 'under-fencing',
        severity: 'warning',
        title: 'Under-fencing risk',
        text: 'You identified an acute and hard-to-substitute physical dependency, but the package does little to diversify qualified supply or bridge a cutoff.' +
          (lv(pkg, 'rnd') > 0 && coverage.buffer === 0 ? ' R&D support alone will not let critical production continue through a cutoff.' : ''),
        policies: ['stockpile', 'publicFinance', 'productionSubsidy'],
      });
    if (isSmallInput && lv(pkg, 'stockpile') === 0)
      add({
        kind: 'remediation-gap',
        severity: 'caution',
        title: 'Remediation gap',
        text: 'A chokepoint is not remediated until a prolonged cutoff could be survived. Nothing in the package bridges the time to restore trusted supply.',
        policies: ['stockpile'],
      });
    if (variant === 'small-control' && (wideSecurity || hardwareRestriction))
      add({
        kind: 'over-fencing',
        severity: 'warning',
        title: 'Over-fencing risk',
        text: 'The vulnerability is isolated to the control layer, but your package restricts the entire hardware stack. A narrow, high fence around firmware and remote access would do the security job at lower cost.',
        policies: (['trustedSource', 'tariff', 'investmentRestriction', 'procurement'] as PolicyId[]).filter((id) => lv(pkg, id) > 0),
      });
    if ((variant === 'small-whole' || variant === 'small-integrated') && secureN > 0 && pkg.securityScope === 'control-layer')
      add({
        kind: 'under-fencing',
        severity: 'caution',
        title: 'Fence too narrow',
        text:
          variant === 'small-whole'
            ? 'The control function cannot currently be separated, so trusted sourcing must cover the whole product; your security measures apply to the control layer only.'
            : 'The control layer is tied to some of its hardware, so the fence must include that hardware; your security measures apply to the control layer only.',
        policies: ['trustedSource', 'cyberStandard'],
      });
    const isLarge = variant === 'large' || variant === 'large-diversify' || variant === 'large-floor';
    if (isLarge && (lv(pkg, 'tariff') >= 2 || lv(pkg, 'procurement') >= 3 || wideSecurity))
      add({
        kind: 'over-fencing',
        severity: 'warning',
        title: 'Over-fencing risk',
        text: 'Neither an acute security problem nor a convincing learning case was found, yet the package raises the cost of a good whose value lies in cheap, fast deployment. State the strategic benefit that justifies the premium, or lower the fence.',
        policies: (['tariff', 'procurement', 'trustedSource'] as PolicyId[]).filter((id) => lv(pkg, id) > 0),
      });
    if ((variant === 'medium' || variant === 'medium-costly') && protectN > buildN && buildN < 3)
      add({
        kind: 'mismatch',
        severity: 'warning',
        title: 'Policy mismatch',
        text: 'You identified industrial capability and learning as the problem, but the package relies mainly on import restrictions and contains little capability-building support (R&D, finance, workforce and supplier development, production support).',
        policies: PROTECT.filter((id) => lv(pkg, id) > 0),
      });
    if ((variant === 'medium' || variant === 'medium-costly') && strategies.has('catchUp') && lv(pkg, 'licensingJV') === 0 && (lv(pkg, 'investmentRestriction') >= 2 || lv(pkg, 'tariff') >= 3))
      add({
        kind: 'closed-gate',
        severity: 'caution',
        title: 'Closed-gate risk',
        text: 'Catching up usually depends on learning from the frontier, but the package shuts out foreign technology and capital without a licensing or joint-venture gate. A medium fence needs gates.',
        policies: ['licensingJV', 'investmentRestriction'],
      });
    if ((variant === 'medium' || variant === 'medium-costly' || variant === 'large-floor') && sum(pkg, ['productionSubsidy', 'tariff', 'procurement']) >= 4 && lv(pkg, 'performanceSunset') === 0)
      add({
        kind: 'no-exit',
        severity: 'caution',
        title: 'No exit',
        text: 'Substantial support with no performance requirements or sunset review risks permanent protection without competitiveness metrics.',
        policies: ['performanceSunset'],
      });
    if ((variant.startsWith('medium') || variant.startsWith('large')) && (lv(pkg, 'exportControls') >= 2 || lv(pkg, 'investmentRestriction') >= 2 || (lv(pkg, 'trustedSource') >= 2 && pkg.securityScope === 'whole-product')))
      add({
        kind: 'misapplied',
        severity: 'caution',
        title: 'Security tools without a security diagnosis',
        text: 'The security tests were not met, so blanket security restrictions do not follow from the diagnosis. If a specific control layer or chokepoint worries you, assess that component separately.',
        policies: (['exportControls', 'investmentRestriction', 'trustedSource'] as PolicyId[]).filter((id) => lv(pkg, id) > 0),
      });
    if (strategies.has('leapAhead') && (variant === 'medium' || variant === 'medium-costly') && lv(pkg, 'rnd') < 2 && lv(pkg, 'publicFinance') === 0)
      add({
        kind: 'leap-unfunded',
        severity: 'caution',
        title: 'Leap-ahead without a path',
        text: 'A leap-ahead strategy needs R&D, demonstration and first-plant finance; the package offers little of either.',
        policies: ['rnd', 'publicFinance'],
      });
  }

  // what the diagnosis marks as not indicated, or as needed but absent, when no specific rule said so
  if (chosenIds.length > 0) {
    const needs = needsFor(variant);
    const flagged = new Set(findings.flatMap((f) => f.policies));
    for (const purpose of Object.keys(needs) as Purpose[]) {
      const ids = POLICIES.filter((p) => p.purpose === purpose && lv(pkg, p.id) > 0).map((p) => p.id);
      if (needs[purpose] === 'avoid' && ids.length && !ids.some((id) => flagged.has(id)))
        add({
          kind: 'misapplied',
          severity: 'caution',
          title: 'Not indicated by the diagnosis',
          text: `${ids.map((id) => POLICY_BY_ID[id].label).join(', ')}: the diagnosis does not call for this kind of instrument (${PURPOSE_TEXT[purpose]}). Say what problem it solves, or drop it.`,
          policies: ids,
        });
      if (needs[purpose] === 'required' && coverage[purpose] === 0 && !findings.some((f) => f.kind === 'under-fencing' || f.kind === 'remediation-gap'))
        add({
          kind: 'mismatch',
          severity: 'caution',
          title: 'Something the diagnosis needs is missing',
          text: `The diagnosis needs instruments that ${PURPOSE_TEXT[purpose]}; the package has none.`,
          policies: POLICIES.filter((p) => p.purpose === purpose).map((p) => p.id),
        });
    }
  }

  const costScore = POLICIES.reduce((s, p) => s + p.buyerCost * lv(pkg, p.id) * (p.id === 'trustedSource' && pkg.securityScope === 'control-layer' ? 0.5 : 1), 0);
  const costPressure = costScore === 0 ? 'none' : costScore <= 2 ? 'low' : costScore <= 5 ? 'moderate' : 'high';
  if (costPressure === 'high' && !findings.some((f) => f.kind === 'over-fencing'))
    add({
      kind: 'cost',
      severity: 'info',
      title: "High cost to buyers",
      text: 'The package raises buyers’ prices substantially. Make the premium explicit and say what evidence would justify keeping it.',
      policies: (['tariff', 'procurement', 'trustedSource'] as PolicyId[]).filter((id) => lv(pkg, id) > 0),
    });

  if (chosenIds.length > 0 && !findings.some((f) => f.severity === 'warning' || f.severity === 'caution'))
    add({
      kind: 'aligned',
      severity: 'ok',
      title: 'Consistent with the diagnosis',
      text: 'No rule flags a mismatch between this package and the diagnosis. Check the success metrics below to know when to revisit it.',
      policies: [],
    });

  const restrictive = RESTRICTIVE.map((id) => lv(pkg, id));
  const chosen: FenceShape = {
    height: Math.max(0, ...restrictive),
    breadth:
      wideSecurity || lv(pkg, 'tariff') > 0 || lv(pkg, 'procurement') > 0 || lv(pkg, 'investmentRestriction') > 0 || lv(pkg, 'exportControls') > 0
        ? 'wide'
        : 'narrow',
    gates: lv(pkg, 'licensingJV') > 0 ? lv(pkg, 'licensingJV') : Math.max(0, 3 - Math.max(0, ...restrictive)),
  };

  return { findings, coverage, needs: needsFor(variant), costPressure, chosen, diagnosed: diagnosedFence(variant) };
}

/** "How we know it worked": the evidence that should be tracked for each diagnosis. */
export function successMetrics(variant: YardVariant | 'indeterminate'): string[] {
  if (variant === 'small-both') return [...successMetrics('small-control').slice(0, 2), ...successMetrics('small-input').slice(0, 2)];
  if (variant.startsWith('small') && variant !== 'small-input')
    return [
      'Share of the installed base running certified, trusted control software or isolated from remote access',
      'Time to detect and patch a firmware vulnerability across the fleet',
      'Availability of certified trusted replacements at deployment scale',
      'Whether the control layer can be swapped without replacing the hardware (the fence can shrink)',
    ];
  if (variant === 'small-input')
    return [
      'Months critical production could continue through a cutoff (inventories plus qualified alternative supply)',
      'Qualified trusted capacity as a share of critical demand, and how fast it can surge',
      'Share of supply held by the largest potentially coercive supplier',
      'Liquidity and survival of sub-tier suppliers through a price war or cutoff',
    ];
  if (variant === 'medium' || variant === 'medium-costly')
    return [
      'Yields, costs and quality relative to the global frontier',
      'Domestic engineers able to diagnose and modify the process without outside assistance',
      'Domestic suppliers qualifying for more demanding parts of the value chain',
      'Process improvements and successor-technology R&D generated domestically',
      'Public support per unit falling as competitiveness improves',
    ];
  if (variant === 'indeterminate')
    return ['The pivotal evidence gaps listed above, filled', 'A reassessment once they are'];
  return [
    'Delivered cost and pace of deployment',
    'Supplier diversity (HHI) and the share held by any potentially coercive supplier',
    variant === 'large-floor' ? 'Whether the domestic floor is preserving the capability it was specified to preserve' : 'Compliance with cross-cutting safeguards (forced labour, safety, trade law)',
  ];
}
