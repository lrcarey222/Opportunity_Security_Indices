// How robust a node's class is, as two small tables: the class under each variant the model runs (each
// supply constraint, each shock semantics, the other scenario, the other shocks; headline settings
// marked) and the existing-relationship growth-bound sensitivity. Each class shown carries the
// PHYSICAL CAPABILITY NOT VALIDATED tag beside it when the export flags it. Shown on a compact's page under
// each node, and on the landing for the nodes outside every compact, where the node in the diversification
// scenario follows the tables ("Managed Interdependence · <status>"; `diversify`; a compact page shows that
// block in the node's own section). The scenario's class reads "not run" where it did not run: a null class
// there is not missing data. With the scenario toggle (2026-10-19, `scenario`: the toggled one) the node's
// three scenarios come first as a narrow three-row bar (a node chip on the landing), and the block leaves out
// the coverage those rows show.

import type { AibMeta, AibNode } from '../contract.ts';
import { FlaggedClass } from '../charts/ClassChip.tsx';
import { fmtPp, fmtShare } from '../derive/format.ts';
import { classVariants, growthRows, HEADLINE_GROWTH } from '../derive/sensitivity.ts';
import type { AibToggle } from '../scenarioUrl.ts';
import { DiversifyBlock, NodeScenarioRows } from './DiversifyBlock.tsx';

export function NodeRobustness({
  n, meta, names = {}, diversify = true, scenario = null,
}: {
  n: AibNode;
  meta: AibMeta;
  names?: Readonly<Record<string, string>>;
  /** show the node in the diversification scenario after the tables */
  diversify?: boolean;
  /** the toggled scenario when the bundle has the scenarios: the node's three scenarios come first */
  scenario?: AibToggle | null;
}) {
  const variants = classVariants(n, meta);
  const growth = growthRows(n);
  const rows =
    diversify && scenario ? (
      <div className="mb-3">
        <p className="mb-1 text-xs text-paper-mute">{n.label} under the three scenarios: kept, restored by trade and new capacity; the figure is the share served after trade.</p>
        <NodeScenarioRows n={n} highlight={scenario} meta={meta} mini />
      </div>
    ) : null;
  const block = diversify && n.diversify !== undefined ? <DiversifyBlock n={n} meta={meta} names={names} scenarios={!!scenario} /> : null;
  if (!variants.length && !growth.length) {
    return (
      <>
        {rows}
        <p className="text-xs text-paper-mute">This export has no variant classes or growth-bound sensitivity for this node.</p>
        {block}
      </>
    );
  }
  return (
    <>
      {rows}
      <div className="grid gap-x-8 gap-y-4 lg:grid-cols-2">
        {variants.length > 0 && (
          <div className="relative overflow-x-auto">
            <table className="data-table text-xs">
              <caption className="mb-1 text-left text-xs text-paper-mute">{n.label}: the class under each variant (headline settings marked)</caption>
              <thead>
                <tr>
                  <th scope="col">Varied setting</th>
                  <th scope="col">Setting</th>
                  <th scope="col">Class</th>
                </tr>
              </thead>
              <tbody>
                {variants.map((v) => (
                  <tr key={`${v.group}-${v.key}`}>
                    <td>{v.group}</td>
                    <td>
                      {v.setting}
                      {v.headline ? ' (headline)' : ''}
                    </td>
                    <td>
                      {v.notRun ? (
                        <span
                          className="text-paper-mute"
                          title="The scenario runs only for a node more concentrated than its benchmark with a supplier above the cap in the allied market"
                          data-testid="variant-not-run"
                        >
                          not run
                        </span>
                      ) : (
                        <FlaggedClass cls={v.cls} warning={v.warning} />
                      )}
                      {v.differs ? <span className="ml-1 text-paper-mute">(differs)</span> : null}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
        {growth.length > 0 && (
          <div className="relative overflow-x-auto">
            <table className="data-table text-xs">
              <caption className="mb-1 text-left text-xs text-paper-mute">
                {n.label}: existing relationships under each growth bound ({HEADLINE_GROWTH} is the headline)
              </caption>
              <thead>
                <tr>
                  <th scope="col">Existing relationship may grow</th>
                  <th scope="col" className="num">
                    Existing coverage
                  </th>
                  <th scope="col" className="num">
                    Coordination dividend
                  </th>
                  <th scope="col">Class</th>
                </tr>
              </thead>
              <tbody>
                {growth.map((g) => (
                  <tr key={g.label}>
                    <td>
                      {g.label} ({g.bound}){g.headline ? ', headline' : ''}
                    </td>
                    <td className="num tabular">{fmtShare(g.existing)}</td>
                    <td className="num tabular">{fmtPp(g.dividend)}</td>
                    <td>
                      <FlaggedClass cls={g.cls} warning={g.warning} />
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
            <p className="mt-1 text-xs text-paper-mute">The sensitivity re-solves existing relationships for coverage alone, so the {HEADLINE_GROWTH} row can differ from the headline figures by a few hundredths of a point.</p>
          </div>
        )}
      </div>
      {block}
    </>
  );
}
