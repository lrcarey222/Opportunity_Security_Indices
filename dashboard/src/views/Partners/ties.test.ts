// The ties chunk's derive functions (views/Partners/ties.ts): bilateral positions with the partner's
// mirror report as a flagged fallback and conduit centres marked; factory-project flows both ways;
// Development Potential by sector; agreements; UN-vote alignment; money formatting; parsing.

import { describe, expect, it } from 'vitest';
import { alignmentOf, bySize, dpOfCell, fmtUsdM, hasCmfdi, positionsOf, projectFlows, sharedAgreements, tiesName } from './ties.ts';
import { syntheticTies } from './tiesFixture.ts';

describe('ties', () => {
  const t = syntheticTies();

  it('parses the chunk', () => {
    expect(t.meta.dip_years).toEqual([2020, 2021, 2022, 2023, 2024]);
    expect(t.dip).toHaveLength(8);
    expect(t.top_all[0]).toMatchObject({ partner_iso: 'JPN', psi_rank: 2 });
    expect(tiesName(t, 'DEU')).toBe('Germany');
    expect(tiesName(t, 'XXX')).toBe('XXX');
    expect(tiesName(null, 'DEU')).toBe('DEU');
  });

  it("positions: the economy's own report first, the partner's mirror report flagged, conduits marked", () => {
    const p = new Map(positionsOf(t, 'USA', 'all').map((r) => [r.partner_iso, r]));
    expect(p.get('JPN')).toMatchObject({ out: 700000, in: 680000, out_mirror: false, in_mirror: false, idx_mean: 45000, conduit: false });
    expect(p.get('DEU')).toMatchObject({ out: 200000, out_year: 2022, in: 650000, in_mirror: true });
    expect(p.get('IND')).toMatchObject({ out: 50000, out_mirror: false });
    expect(p.get('NLD')?.conduit).toBe(true);
    expect(bySize([...p.values()]).map((r) => r.partner_iso)).toEqual(['JPN', 'NLD', 'DEU', 'IND']);
    const debt = positionsOf(t, 'USA', 'debt');
    expect(debt.map((r) => r.partner_iso)).toEqual(['JPN']);
    expect(debt[0].out).toBe(50000);
  });

  it('factory projects in one sector, both directions', () => {
    expect(hasCmfdi('Solar', 'Midstream')).toBe(true);
    expect(hasCmfdi('Solar', 'Upstream')).toBe(false);
    const f = projectFlows(t, 'USA', 'Solar', 'Midstream');
    expect(f.map((r) => r.partner_iso)).toEqual(['JPN', 'DEU']);
    expect(f[0]).toMatchObject({ out_projects: 1, out_value: null, in_projects: 3, in_value: 500, in_disclosed: 2 });
    expect(f[1]).toMatchObject({ out_projects: 2, out_value: 80, in_projects: 0 });
    expect(projectFlows(t, 'USA', 'Wind', 'Midstream')[0]).toMatchObject({ partner_iso: 'JPN', in_projects: 5 });
  });

  it('Development Potential by sector, shared agreements, UN-vote alignment', () => {
    expect(dpOfCell(t, 2).get('IND')).toBe(0.7);
    expect(dpOfCell(t, 3).size).toBe(0);
    expect(sharedAgreements(t, 'USA', 'JPN')).toEqual(['IPEF', 'MSP']);
    expect(sharedAgreements(t, 'USA', 'DEU')).toEqual([]);
    expect(alignmentOf(t, 'USA', 'JPN')).toBe(0.8);
    expect(alignmentOf(t, 'JPN', 'USA')).toBeNull();
    expect(alignmentOf(t, 'USA', 'XXX')).toBeNull();
  });

  it('formats US$ millions in words', () => {
    expect(fmtUsdM(700000)).toBe('$700 billion');
    expect(fmtUsdM(1234)).toBe('$1.2 billion');
    expect(fmtUsdM(450)).toBe('$450 million');
    expect(fmtUsdM(0.4)).toBe('$0.4 million');
    expect(fmtUsdM(-2500)).toBe('−$2.5 billion');
    expect(fmtUsdM(5.2e6)).toBe('$5.2 trillion');
    expect(fmtUsdM(null)).toBe('–');
  });
});
