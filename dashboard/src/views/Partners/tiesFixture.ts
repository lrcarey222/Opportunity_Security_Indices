// A small synthetic ties chunk for the tests (ties.test.ts, partners.test.tsx). Not imported by the app.

import { table } from '../../engine/table.ts';
import { parseTies, type Ties } from './ties.ts';

export function syntheticTies(): Ties {
  const dip = (reporter_iso: string, partner_iso: string, measure: string, value: number | null, year = 2023, mean_idx: number | null = null) => ({ reporter_iso, partner_iso, measure, year, value, mean_idx });
  return parseTies({
    meta: { dip_years: [2020, 2021, 2022, 2023, 2024], dip_gdp_year: 2023, dip_unit: 'US$ millions', dip_source: 'IMF, Direct Investment Positions (DIP), reported official data', conduit_rule: 'conduits', conduit_scope: 'both', top_k: 10 },
    U: ['USA', 'JPN', 'DEU', 'NLD', 'IND'],
    U_names: ['United States', 'Japan', 'Germany', 'Netherlands', 'India'],
    dip: table({
      ...Object.fromEntries(
        (['reporter_iso', 'partner_iso', 'measure', 'year', 'value', 'mean_idx'] as const).map((k) => [
          k,
          (() => {
            const rows = [
              dip('USA', 'JPN', 'out_all', 700000, 2023),
              dip('USA', 'JPN', 'in_all', 680000, 2023),
              dip('USA', 'JPN', 'out_debt', 50000, 2023, 45000),
              dip('USA', 'NLD', 'out_all', 900000, 2023),
              dip('USA', 'DEU', 'out_all', 200000, 2022),
              // the USA reports nothing inward from Germany: Germany's own outward report is used
              dip('DEU', 'USA', 'out_all', 650000, 2023),
              // India reports an inward position from the USA: the USA's own report wins
              dip('IND', 'USA', 'in_all', 1, 2023),
              dip('USA', 'IND', 'out_all', 50000, 2023),
            ];
            const v = rows.map((r) => r[k]);
            return k === 'year' ? Int32Array.from(v as number[]) : k === 'value' || k === 'mean_idx' ? Float64Array.from(v as (number | null)[], (x) => (x === null ? NaN : x)) : (v as (string | null)[]);
          })(),
        ]),
      ),
    }),
    conduits: table({ iso3: ['NLD', 'BHS'], name: ['Netherlands', 'Bahamas'], dej_top10: Uint8Array.from([1, 0]), in_rule: Uint8Array.from([1, 1]) }),
    cmfdi: table({
      investor_iso: ['JPN', 'USA', 'USA', 'JPN'], destination_iso: ['USA', 'JPN', 'DEU', 'USA'], tech: ['Solar', 'Solar', 'Solar', 'Wind'], supply_chain: ['Midstream', 'Midstream', 'Midstream', 'Midstream'],
      n_projects: Float64Array.from([3, 1, 2, 5]), n_disclosed: Float64Array.from([2, 0, 1, 5]), value_usd_m: Float64Array.from([500, NaN, 80, 900]),
    }),
    dp: table({ partner_iso: ['IND', 'IND'], cell_id: Int32Array.from([1, 2]), value: Float64Array.from([0.4, 0.7]) }),
    agreements: table({ iso3: ['USA', 'JPN', 'USA', 'IND', 'JPN'], agreement: ['MSP', 'MSP', 'IPEF', 'IPEF', 'IPEF'] }),
    geo: { shape: [5, 5], data: Float64Array.from({ length: 25 }, (_, i) => (i === 0 + 5 * 1 ? 0.8 : NaN)) },
    top_all: table({ reporter_iso: ['USA'], side: ['fs'], k: Int32Array.from([1]), partner_iso: ['JPN'], value: Float64Array.from([0.9]), rank: Int32Array.from([1]), n: Int32Array.from([4]), psi_rank: Int32Array.from([2]) }),
  });
}
