// The basis of the posture blocks (design L6, 2026-10-28): "Strategic position framework", a basis of its
// own beside the pages' "Observed / modelled facts" and "Preference-adjusted priorities". The framework
// classifies each node at fixed thresholds, so neither the Perspective dials nor the AIB scenario toggle
// changes it. The tag has the same markup as the Country profile's BasisTag (`.tag`, data-basis), so a
// page's Section can show it beside its own tags. Also: the glossary ids the posture cards link with Term
// (content/glossary.ts gains the entries; until then Term shows the words without a tip).

/** The basis label of every posture block, on the Country, Sector and AIB pages alike. */
export const POSTURE_BASIS_LABEL = 'Strategic position framework';

/** The tag's default note. */
export const POSTURE_BASIS_NOTE = 'fixed thresholds; the dials and the scenario do not change it';

/** The tag's tooltip: the same in a sentence. */
export const POSTURE_BASIS_TITLE =
  'The strategic position framework classifies each node at fixed thresholds of Strategic Need and Industrial Opportunity. Neither the Perspective dials nor the scenario toggle changes it.';

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
 * The glossary ids of the posture vocabulary (to be added to content/glossary.ts; 'alignment' there is
 * UN-vote alignment and 'need' the Strategic Index's security need, so these ids are new). The cards use
 * them with Term and explicit words, so a missing entry shows the words without a tip.
 */
export const POSTURE_TERMS = {
  posture: 'posture',
  strategicNeed: 'strategic_need',
  industrialOpportunity: 'industrial_opportunity',
  competitivePosition: 'competitive_position',
  securityRationale: 'security_rationale',
  policyAlignment: 'policy_alignment',
  partnerGap: 'partner_gap',
  tradeMomentum: 'trade_momentum',
  aibGap: 'aib_gap',
  partnerDemand: 'partner_demand',
} as const;
