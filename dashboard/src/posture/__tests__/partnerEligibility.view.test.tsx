// The drawer's partner block (PartnerEligibilityList): eligible trusted partners first, suppliers outside the
// trusted network apart and never recommended, a plain line when no partner is eligible, and the engine's
// reason line only when every listed partner is a member.

import { cleanup, render, screen } from '@testing-library/react';
import { afterEach, describe, expect, it } from 'vitest';
import { PartnerEligibilityList } from '../PartnerEligibilityList.tsx';

afterEach(cleanup);

const core = {
  countries: [
    { iso3: 'AUS', name: 'Australia', tier: 'core' },
    { iso3: 'VNM', name: 'Viet Nam', tier: 'partner' },
    { iso3: 'CHN', name: 'China', tier: 'none' },
    { iso3: 'RUS', name: 'Russia', tier: 'none' },
  ],
};
const names = Object.fromEntries(core.countries.map((c) => [c.iso3, c.name]));
const REASON = 'CHN holds 37% of allied supply (AIB), relationship 0.94. Only 1 credible trusted supplier.';

describe('PartnerEligibilityList', () => {
  it('a member and a non-member: the member is the eligible partner; China is listed outside, not recommended; no engine reason', () => {
    render(
      <PartnerEligibilityList
        core={core}
        partners={[{ iso3: 'VNM', capability: 0.12, cp: 0.9, relationship: 0.8 }, { iso3: 'CHN', capability: 0.6, cp: 0.99, relationship: 0.9 }]}
        partner1="VNM"
        gap={false}
        reason={REASON}
        names={names}
      />,
    );
    const eligible = screen.getByTestId('partner-eligible');
    expect(eligible.textContent).toContain('1. Viet Nam');
    expect(eligible.textContent).toContain('holds 12% of supply · position 90% · strong relationship');
    expect(eligible.textContent).not.toContain('China');
    const outside = screen.getByTestId('partner-outside');
    expect(outside.textContent).toContain('Major suppliers outside the trusted network (not recommended as partners)');
    expect(outside.textContent).toContain('China');
    expect(outside.textContent).not.toContain('relationship');
    expect(document.body.textContent).not.toContain('0.94');
    expect(document.body.textContent).not.toContain('credible trusted supplier');
  });

  it('only non-members: no eligible partner, said plainly, with the suppliers named', () => {
    render(<PartnerEligibilityList core={core} partners={[{ iso3: 'RUS', capability: 0.48 }, { iso3: 'CHN', capability: 0.2 }]} partner1="RUS" gap={false} reason={REASON} names={names} />);
    expect(screen.getByTestId('partner-none-eligible').textContent).toBe(
      'No eligible trusted partner identified; the leading suppliers, Russia and China, are outside the trusted network.',
    );
    expect(screen.queryByTestId('partner-eligible')).toBeNull();
    expect(screen.getByTestId('partner-outside').textContent).toContain('Russia');
    expect(document.body.textContent).not.toContain(REASON);
  });

  it('members only: the list and the engine reason; the core first partner while the detail loads; the gap', () => {
    render(<PartnerEligibilityList core={core} partners={[{ iso3: 'AUS', capability: 0.4 }]} partner1="AUS" gap={false} reason="AUS holds 40% of world supply." names={names} />);
    expect(screen.getByTestId('partner-eligible').textContent).toContain('Australia');
    expect(screen.queryByTestId('partner-outside')).toBeNull();
    expect(document.body.textContent).toContain('AUS holds 40% of world supply.');
    cleanup();
    render(<PartnerEligibilityList core={core} partners={null} partner1="CHN" gap={false} reason={null} names={names} />);
    expect(screen.getByTestId('partner-outside').textContent).toBe('First-ranked supplier: China, outside the trusted network');
    cleanup();
    render(<PartnerEligibilityList core={core} partners={null} partner1="AUS" gap={false} reason={null} names={names} />);
    expect(document.body.textContent).toBe('Best-placed partner: Australia');
    cleanup();
    render(<PartnerEligibilityList core={core} partners={[]} partner1={null} gap reason={null} names={names} />);
    expect(document.body.textContent).toBe('No credible allied solution yet: no trusted supplier can cover it.');
  });
});
