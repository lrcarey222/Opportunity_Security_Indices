// The basis of the posture blocks (design L6, 2026-10-28; reworded 2026-10-29 for the canonical framework):
// "Strategic position framework", a basis of its own beside the pages' "Observed / modelled facts" and
// "Preference-adjusted priorities". The framework decides each node at fixed thresholds (Need and Prize
// triage, Competitive Position decides, Secure overrides, Deployment & Use reads deployment need), so
// neither the Perspective dials nor the AIB scenario toggle changes it. The tag has the same markup as the
// Country profile's BasisTag (`.tag`, data-basis), so a page's Section can show it beside its own tags.
// Also: the glossary ids the posture cards link with Term (content/glossary.ts holds the entries; a missing
// entry shows the words without a tip).

/** The basis label of every posture block, on the Country, Sector and AIB pages alike. */
export const POSTURE_BASIS_LABEL = 'Strategic position framework';

/** The tag's default note. */
export const POSTURE_BASIS_NOTE = 'fixed thresholds; the dials and the scenario do not change it';

/** The tag's tooltip: the same in a sentence. */
export const POSTURE_BASIS_TITLE =
  'The strategic position framework makes one choice for each supply-chain step at fixed thresholds of Need, Strategic Industrial Value (the prize) and Competitive Position, after the Secure tests; Deployment & Use reads deployment need. Neither the Perspective dials nor the scenario toggle changes it.';

/** "Strategic position framework · fixed thresholds; ..." as a quiet tag (pass note={null} for the label alone). */
export function PostureBasisTag({ note = POSTURE_BASIS_NOTE }: { note?: string | null }) {
  return (
    // `.tag` is unlayered CSS with white-space: nowrap; the inline style lets a long note wrap on a phone
    <span className="tag max-w-full" style={{ whiteSpace: 'normal' }} data-basis="posture" title={POSTURE_BASIS_TITLE}>
      {POSTURE_BASIS_LABEL}
      {note ? ` · ${note}` : ''}
    </span>
  );
}

/**
 * The glossary ids of the posture vocabulary ('alignment' in the glossary is UN-vote alignment and 'need'
 * the Strategic Index's security need, so these ids are their own). The cards use them with Term and
 * explicit words, so a missing entry shows the words without a tip; content/glossary.test.ts checks that
 * every id here has an entry.
 */
export const POSTURE_TERMS = {
  posture: 'posture',
  strategicNeed: 'strategic_need',
  supplyChainNeed: 'supply_chain_need',
  deploymentNeed: 'deployment_need',
  needType: 'need_type',
  /** the prize the choice reads (2026-10-30): market prize, industrial commons and future option value */
  strategicValue: 'strategic_industrial_value',
  /** the market prize alone: one family of the Strategic Industrial Value, and the legacy prize measure */
  industrialOpportunity: 'industrial_opportunity',
  /** the yards of the Yards & Fences framing (small, medium, large, mixed) */
  yard: 'yard',
  /** the countervailing test of a Large yard: commodity signals, whatever the size of the market */
  largeYard: 'large_yard_case',
  competitivePosition: 'competitive_position',
  pathway: 'pathway',
  deployMode: 'deploy_mode',
  securityRationale: 'security_rationale',
  deliveryMode: 'delivery_mode',
  consistencyFlag: 'consistency_flag',
  policyAlignment: 'policy_alignment',
  partnerGap: 'partner_gap',
  tradeMomentum: 'trade_momentum',
  aibGap: 'aib_gap',
  partnerDemand: 'partner_demand',
} as const;
