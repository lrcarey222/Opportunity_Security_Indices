// The Strategic gaps visuals (docs/partnership_agreements.md section 8):
//   * GapMatrixCard: partner x technology, one cell per gap row of the chosen side, coloured and marked by its
//     class (established, opportunity, legacy); a chosen cell opens its reading and its stage strip;
//   * ExecutionGapCard: technology x supply stage, the priority stages no agreement covers, with the number of
//     capable, aligned suppliers;
//   * PairGapStrips: the pair's technologies x supply stages, in each direction.
// Colours (validated with the dataviz validator against the card surface ink-800 #1f2937 on 2026-11-07: lightness
// band, chroma, CVD and normal-vision separation, contrast all pass): established = brand blue #1889b0,
// opportunity = brand orange step #e46f1c. Legacy is an outline, never a grey hue. Every cell also carries a glyph and
// words, so colour never carries the class alone.

import { useMemo, useState, type ReactNode } from "react";
import { Link } from "react-router";
import { ChartCard } from "../../charts/ChartCard.tsx";
import { Segmented } from "../../components/ui.tsx";
import type { AgrExecutionGap, AgrGap, GapSide } from "../contract.ts";
import {
  executionGapText,
  executionStageRows,
  GAP_STAGE_SHORT,
  GAP_STAGES,
  gapCellWords,
  gapComponents,
  gapMatrix,
  gapText,
  plural,
  QUADRANT_NAME,
  QUADRANT_TITLE,
  SHOWN_QUADRANTS,
  STAGE_STATE_WORDS,
  stageListWords,
  stageStates,
  TECH_SHORT,
  techOrder,
  type GapContext,
  type ShownQuadrant,
  type StageState,
} from "../derive.ts";
import { AbsenceCaveat, NoneFound } from "./parts.tsx";

export const GAP_FILL = {
  established: "#1889b0",
  opportunity: "#e46f1c",
} as const;
const GAP_INK = { established: "#ffffff", opportunity: "#111827" } as const;
export const GAP_GLYPH: Record<ShownQuadrant, string> = {
  established: "●",
  opportunity: "○",
  legacy: "◇",
};
const GAP_COUNT_WORDS: Record<ShownQuadrant, [string, string]> = {
  opportunity: ["opportunity", "opportunities"],
  established: ["established", "established"],
  legacy: ["legacy", "legacy"],
};
const GAP_MEANING: Record<ShownQuadrant, string> = {
  opportunity: "complementary, no established agreement",
  established: "complementary and covered by an agreement",
  legacy: "covered by an agreement, no complementarity measured",
};
/** Rows shown before "Show all" (the partners with the most opportunities first). */
export const GAP_MATRIX_TOP = 15;

const cellStyle = (q: ShownQuadrant) =>
  q === "legacy"
    ? { boxShadow: "inset 0 0 0 1px #9ca3af", color: "#d1d5db" }
    : { background: GAP_FILL[q], color: GAP_INK[q] };

const stageStyle = (s: StageState) =>
  s === "gap"
    ? { background: GAP_FILL.opportunity, color: GAP_INK.opportunity }
    : s === "covered"
      ? { background: GAP_FILL.established, color: GAP_INK.established }
      : s === "covered_only"
        ? {
            boxShadow: `inset 0 0 0 2px ${GAP_FILL.established}`,
            color: "#d1d5db",
          }
        : { boxShadow: "inset 0 0 0 1px #374151", color: "#6b7280" };

const STAGE_GLYPH: Record<StageState, string> = {
  gap: "○",
  covered: "●",
  covered_only: "◇",
  none: "",
};

export function GapLegend() {
  return (
    <ul
      className="flex flex-wrap gap-x-5 gap-y-1 text-xs text-paper-dim"
      data-testid="gap-legend"
    >
      {SHOWN_QUADRANTS.map((q) => (
        <li key={q} className="flex items-center gap-1.5">
          <span
            aria-hidden="true"
            className="inline-flex h-4 w-4 items-center justify-center rounded-sm text-[0.65rem]"
            style={cellStyle(q)}
          >
            {GAP_GLYPH[q]}
          </span>
          <span>
            <span className="text-paper">{QUADRANT_NAME[q]}</span>:{" "}
            {GAP_MEANING[q]}
          </span>
        </li>
      ))}
    </ul>
  );
}

export function StageLegend() {
  const states: StageState[] = ["gap", "covered", "covered_only"];
  return (
    <ul
      className="flex flex-wrap gap-x-5 gap-y-1 text-xs text-paper-dim"
      data-testid="stage-legend"
    >
      {states.map((s) => (
        <li key={s} className="flex items-center gap-1.5">
          <span
            aria-hidden="true"
            className="inline-flex h-4 w-6 items-center justify-center rounded-sm text-[0.65rem]"
            style={stageStyle(s)}
          >
            {STAGE_GLYPH[s]}
          </span>
          <span>{STAGE_STATE_WORDS[s]}</span>
        </li>
      ))}
    </ul>
  );
}

/** One gap row's supply stages in a row of small cells. */
export function StageStrip({ g, label }: { g: AgrGap; label?: string }) {
  const st = stageStates(g);
  return (
    <div
      className="flex gap-0.5"
      role="list"
      aria-label={label ?? `${g.tech} by supply stage`}
      data-testid="stage-strip"
    >
      {GAP_STAGES.map((s) => (
        <span
          key={s}
          role="listitem"
          className="inline-flex h-6 w-10 flex-col items-center justify-center rounded-sm text-[0.6rem] leading-none"
          style={stageStyle(st[s])}
          title={`${s}: ${STAGE_STATE_WORDS[st[s]]}`}
          data-state={st[s]}
        >
          <span aria-hidden="true">{GAP_STAGE_SHORT[s]}</span>
          <span className="sr-only">
            {s}: {STAGE_STATE_WORDS[st[s]]}
          </span>
        </span>
      ))}
    </div>
  );
}

const keyOf = (g: AgrGap) => `${g.partner}|${g.tech}|${g.side}`;

/** The reading of one chosen gap row: class, sentence, stage strip, the relationship link. */
function GapDetail({
  g,
  ctx,
  href,
}: {
  g: AgrGap;
  ctx: GapContext;
  href: string;
}) {
  const q = g.quadrant as ShownQuadrant;
  return (
    <div
      className="mt-4 rounded-md border border-ink-600 p-3"
      data-testid="gap-detail"
      aria-live="polite"
    >
      <p className="text-xs text-paper-mute">
        <span
          aria-hidden="true"
          className="mr-1.5 inline-flex h-4 w-4 items-center justify-center rounded-sm align-middle text-[0.65rem]"
          style={cellStyle(q)}
        >
          {GAP_GLYPH[q]}
        </span>
        <span className="text-paper">{QUADRANT_NAME[q]}</span> ·{" "}
        {QUADRANT_TITLE[q].toLowerCase()} · {g.tech}
      </p>
      <p className="mt-2 text-sm text-paper-dim">{gapText(g, ctx)}</p>
      <div className="mt-3 flex flex-wrap items-center gap-x-4 gap-y-2">
        <StageStrip
          g={g}
          label={`${g.tech}, ${ctx.name(g.partner)}: by supply stage`}
        />
        <StageLegend />
      </div>
      <p className="mt-3 text-xs">
        <Link className="link" to={href}>
          {ctx.name(ctx.focal)} and {ctx.name(g.partner)}: the relationship
          &rarr;
        </Link>
      </p>
    </div>
  );
}

function sideWords(side: GapSide, focal: string) {
  return side === "needs"
    ? `Who could supply ${focal}`
    : `Where ${focal} could supply`;
}

/** The table twin: every row of the side, with its reading. */
function GapTable({ rows, ctx }: { rows: AgrGap[]; ctx: GapContext }) {
  return (
    <div className="relative overflow-x-auto">
      <table className="data-table text-xs">
        <caption className="sr-only">Strategic gaps</caption>
        <thead>
          <tr>
            <th scope="col">Partner</th>
            <th scope="col">Technology</th>
            <th scope="col">Class</th>
            <th scope="col">Complementary at</th>
            <th scope="col">Covered at</th>
            <th scope="col">Components</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((g) => (
            <tr key={keyOf(g)}>
              <td>{ctx.name(g.partner)}</td>
              <td>{g.tech}</td>
              <td>{QUADRANT_NAME[g.quadrant]}</td>
              <td>{stageListWords(g.stages_high) || "—"}</td>
              <td>{stageListWords(g.stages_covered) || "—"}</td>
              <td>{gapComponents(g, ctx.compacts)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

const csvCell = (x: string | number | null | undefined) => {
  const s = x === null || x === undefined ? "" : String(x);
  return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
};

export function GapMatrixCard({
  gaps,
  focal,
  names,
  ctx,
  hrefOf,
  tech,
  title = "Strategic gaps",
}: {
  gaps: readonly AgrGap[];
  focal: string;
  names: Readonly<Record<string, string>>;
  ctx: GapContext;
  hrefOf: (iso: string, partner?: string | null) => string;
  tech: string | null;
  title?: string;
}) {
  const [side, setSide] = useState<GapSide>("needs");
  const [chosen, setChosen] = useState<string | null>(null);
  const [all, setAll] = useState(false);
  const model = useMemo(
    () => gapMatrix(gaps, side, names),
    [gaps, side, names],
  );
  const shown = all ? model.rows : model.rows.slice(0, GAP_MATRIX_TOP);
  const rowsFlat = useMemo(
    () =>
      model.rows.flatMap((r) =>
        model.techs.map((t) => r.cells[t]).filter(Boolean),
      ),
    [model],
  );
  // the chosen cell, else the first opportunity (or the first cell) of the side
  const selected =
    rowsFlat.find((g) => keyOf(g) === chosen) ??
    rowsFlat.find((g) => g.quadrant === "opportunity") ??
    rowsFlat[0] ??
    null;
  const fname = ctx.name(focal);
  const sideLabel = sideWords(side, fname);
  const summary = `${sideLabel}: ${plural(model.counts.opportunity, "opportunity", "opportunities")}, ${plural(model.counts.established, "established cell")}, ${plural(model.counts.legacy, "legacy cell")} across ${plural(model.rows.length, "partner")}.`;
  return (
    <ChartCard
      title={title}
      sub={
        <>
          Where {fname} and a partner complement each other in a technology, and
          whether an agreement covers the stages where they do. Each cell is a
          labelled rule over need, capability, recommendations, UN-vote
          alignment and coverage, not a score. Choose a cell to read it.
          {tech ? ` ${tech} only.` : ""}
        </>
      }
      tools={
        <Segmented<GapSide>
          label="Direction"
          hideLabel
          value={side}
          options={[
            { value: "needs", label: `Who could supply ${fname}` },
            { value: "supplies", label: `Where ${fname} could supply` },
          ]}
          onChange={(s) => {
            setSide(s);
            setChosen(null);
            setAll(false);
          }}
        />
      }
      legend={<GapLegend />}
      notes={<AbsenceCaveat />}
      summary={summary}
      table={() => <GapTable rows={rowsFlat} ctx={ctx} />}
      csv={() => ({
        name: `strategic-gaps-${focal}-${side}`,
        text: [
          [
            "partner",
            "technology",
            "class",
            "complementary_at",
            "covered_at",
            "need",
            "capability",
            "recommended_cells",
            "alignment",
          ].join(","),
          ...rowsFlat.map((g) =>
            [
              g.partner,
              g.tech,
              g.quadrant,
              (g.stages_high ?? []).join("|"),
              (g.stages_covered ?? []).join("|"),
              g.need,
              g.capability,
              g.recommended,
              g.alignment ?? null,
            ]
              .map(csvCell)
              .join(","),
          ),
        ].join("\n"),
      })}
    >
      <div data-testid="strategic-gaps">
        <p className="text-sm text-paper-dim" data-testid="gap-counts">
          {SHOWN_QUADRANTS.map((q, i) => (
            <span key={q}>
              {i > 0 && " · "}
              <span aria-hidden="true" className="mr-1">
                {GAP_GLYPH[q]}
              </span>
              <span className="tabular text-paper">{model.counts[q]}</span>{" "}
              {model.counts[q] === 1
                ? GAP_COUNT_WORDS[q][0]
                : GAP_COUNT_WORDS[q][1]}
            </span>
          ))}
          <span className="text-paper-mute">
            {" "}
            · {plural(model.rows.length, "partner")}
          </span>
        </p>
        {model.rows.length ? (
          <>
            <div className="relative mt-3 overflow-x-auto" data-testid="gap-matrix">
              <table className="border-collapse text-xs">
                <caption className="sr-only">{summary}</caption>
                <thead>
                  <tr>
                    <th
                      scope="col"
                      className="sticky left-0 bg-ink-800 px-1 text-left align-bottom font-normal text-paper-mute"
                    >
                      Partner
                    </th>
                    {model.techs.map((t) => (
                      <th
                        key={t}
                        scope="col"
                        className="h-20 w-9 px-0 align-bottom font-normal text-paper-mute lg:w-11"
                      >
                        <span className="inline-block origin-bottom-left translate-x-4 -rotate-60 whitespace-nowrap">
                          <abbr title={t} className="no-underline">
                            {TECH_SHORT[t] ?? t}
                          </abbr>
                        </span>
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {shown.map((r) => (
                    <tr key={r.partner}>
                      <th
                        scope="row"
                        className="sticky left-0 max-w-[10rem] truncate whitespace-nowrap bg-ink-800 pr-2 text-left font-normal text-paper-dim"
                        title={r.name}
                      >
                        {r.name}
                      </th>
                      {model.techs.map((t) => {
                        const g = r.cells[t];
                        if (!g)
                          return (
                            <td
                              key={t}
                              className="h-8 w-9 border border-ink-800 lg:h-9 lg:w-11"
                              aria-label={`${r.name}, ${t}: no gap row`}
                            />
                          );
                        const q = g.quadrant as ShownQuadrant;
                        const words = gapCellWords(g, focal, ctx.name);
                        const on =
                          selected !== null && keyOf(selected) === keyOf(g);
                        return (
                          <td
                            key={t}
                            className="h-8 w-9 border border-ink-800 p-0 lg:h-9 lg:w-11"
                          >
                            <button
                              type="button"
                              className={`flex h-8 w-9 items-center justify-center text-sm lg:h-9 lg:w-11 focus-visible:outline-2 focus-visible:outline-offset-[-2px] focus-visible:outline-white ${on ? "outline-2 outline-offset-[-2px] outline-white" : ""}`}
                              style={cellStyle(q)}
                              title={words}
                              aria-label={words}
                              aria-pressed={on}
                              data-quadrant={q}
                              onClick={() => setChosen(keyOf(g))}
                            >
                              <span aria-hidden="true">{GAP_GLYPH[q]}</span>
                            </button>
                          </td>
                        );
                      })}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            {model.rows.length > GAP_MATRIX_TOP && (
              <button
                type="button"
                className="btn-quiet mt-2"
                onClick={() => setAll((x) => !x)}
              >
                {all
                  ? `Show the first ${GAP_MATRIX_TOP}`
                  : `Show all ${model.rows.length} partners`}
              </button>
            )}
            {selected && (
              <GapDetail
                g={selected}
                ctx={ctx}
                href={hrefOf(focal, selected.partner)}
              />
            )}
          </>
        ) : (
          <NoneFound>
            No strategic gap for{" "}
            {sideLabel.charAt(0).toLowerCase() + sideLabel.slice(1)}
            {tech ? ` in ${tech}` : ""}.
          </NoneFound>
        )}
      </div>
    </ChartCard>
  );
}

/** Execution gaps: technology x supply stage, the priority stages without an established agreement with a capable,
 *  aligned supplier; each such cell shows how many suppliers there are. A chosen row opens its reading. */
export function ExecutionGapCard({
  execution,
  ctx,
  tech,
}: {
  execution: readonly AgrExecutionGap[];
  ctx: GapContext;
  tech: string | null;
}) {
  const rows = useMemo(() => executionStageRows(execution), [execution]);
  const [chosen, setChosen] = useState<string | null>(null);
  const sel = rows.find((r) => r.tech === chosen) ?? rows[0] ?? null;
  const fname = ctx.name(ctx.focal);
  return (
    <ChartCard
      title="Execution gaps"
      headingLevel={2}
      sub={`Stages ${fname} prioritises where capable, aligned suppliers exist but no agreement in these sources with any of them covers the stage at an established level. The number is how many such suppliers there are. Choose a row to read it.`}
      legend={
        rows.length ? (
          <ul className="flex flex-wrap gap-x-5 text-xs text-paper-dim">
            <li className="flex items-center gap-1.5">
              <span
                aria-hidden="true"
                className="inline-flex h-4 w-6 items-center justify-center rounded-sm text-[0.6rem]"
                style={stageStyle("gap")}
              >
                n
              </span>
              priority stage without an established agreement (n capable
              suppliers)
            </li>
          </ul>
        ) : undefined
      }
      notes={rows.length ? <AbsenceCaveat /> : undefined}
      table={
        rows.length
          ? () => (
              <ul className="space-y-1.5 text-sm text-paper-dim">
                {rows.map((r) => (
                  <li key={r.tech}>{executionGapText(r.gap, ctx)}</li>
                ))}
              </ul>
            )
          : undefined
      }
    >
      <div data-testid="execution-gaps">
        {rows.length ? (
          <>
            <div className="relative overflow-x-auto">
              <table
                className="border-collapse text-xs"
                data-testid="execution-matrix"
              >
                <caption className="sr-only">
                  Execution gaps of {fname} by technology and supply stage
                </caption>
                <thead>
                  <tr>
                    <th
                      scope="col"
                      className="px-1 text-left font-normal text-paper-mute"
                    >
                      Technology
                    </th>
                    {GAP_STAGES.map((s) => (
                      <th
                        key={s}
                        scope="col"
                        className="w-12 px-0 text-center font-normal text-paper-mute"
                      >
                        <abbr title={s} className="no-underline">
                          {GAP_STAGE_SHORT[s]}
                        </abbr>
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {rows.map((r) => {
                    const n = r.gap.suppliers_without_coverage.length;
                    const on = sel?.tech === r.tech;
                    return (
                      <tr key={r.tech}>
                        <th
                          scope="row"
                          className="whitespace-nowrap pr-2 text-left font-normal"
                        >
                          <button
                            type="button"
                            className={`link text-left ${on ? "text-paper" : ""}`}
                            aria-pressed={on}
                            onClick={() => setChosen(r.tech)}
                          >
                            {r.tech}
                          </button>
                        </th>
                        {GAP_STAGES.map((s) => {
                          const gap = r.stages.has(s);
                          const words = gap
                            ? `${r.tech} at ${s}: a priority without an established agreement; ${plural(n, "capable, aligned supplier")}`
                            : `${r.tech} at ${s}: no execution gap`;
                          return (
                            <td
                              key={s}
                              className="h-7 w-12 border border-ink-800 p-0 text-center tabular"
                              style={gap ? stageStyle("gap") : undefined}
                              title={words}
                              data-gap={gap ? "yes" : "no"}
                            >
                              {gap && <span aria-hidden="true">{n}</span>}
                              <span className="sr-only">{words}</span>
                            </td>
                          );
                        })}
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
            {sel && (
              <p
                className="mt-3 rounded-md border border-ink-600 p-3 text-sm text-paper-dim"
                data-testid="execution-detail"
                aria-live="polite"
              >
                {executionGapText(sel.gap, ctx)}
              </p>
            )}
          </>
        ) : (
          <p className="text-sm text-paper-mute">
            No execution gap recorded{tech ? ` in ${tech}` : ""}.
          </p>
        )}
      </div>
    </ChartCard>
  );
}

/** The pair's gaps: for each direction, its technologies x supply stages, the class of each technology at the left.
 *  A chosen row opens its reading. */
export function PairGapStrips({
  gaps,
  focal,
  partner,
  ctx,
}: {
  gaps: readonly AgrGap[];
  focal: string;
  partner: string;
  ctx: GapContext;
}) {
  const sides: GapSide[] = ["needs", "supplies"];
  const bySide = useMemo(
    () =>
      sides.map((side) => {
        const rows = gaps.filter(
          (g) =>
            g.partner === partner &&
            g.side === side &&
            (SHOWN_QUADRANTS as readonly string[]).includes(g.quadrant),
        );
        const order = techOrder(rows.map((g) => g.tech));
        return {
          side,
          rows: order.map((t) => rows.find((g) => g.tech === t)!),
        };
      }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [gaps, partner],
  );
  const first = bySide.flatMap((s) => s.rows)[0] ?? null;
  const [chosen, setChosen] = useState<string | null>(null);
  const sel =
    bySide.flatMap((s) => s.rows).find((g) => keyOf(g) === chosen) ?? first;
  const [p, f] = [ctx.name(partner), ctx.name(focal)];
  const heading = (side: GapSide): ReactNode =>
    side === "needs" ? `${p} could supply ${f}` : `${f} could supply ${p}`;
  if (!first) return null;
  return (
    <div data-testid="pair-gap-strips">
      <div className="grid gap-5 md:grid-cols-2">
        {bySide.map(({ side, rows }) => (
          <div key={side} data-testid={`pair-gaps-${side}`}>
            <h3 className="text-sm font-medium text-paper">{heading(side)}</h3>
            {rows.length ? (
              <table className="mt-2 border-collapse text-xs">
                <caption className="sr-only">
                  {heading(side)}: technologies by supply stage
                </caption>
                <thead>
                  <tr>
                    <th
                      scope="col"
                      className="pr-2 text-left font-normal text-paper-mute"
                    >
                      Technology
                    </th>
                    <th scope="col" className="sr-only">
                      Class
                    </th>
                    <th
                      scope="col"
                      className="text-left font-normal text-paper-mute"
                    >
                      By supply stage
                    </th>
                  </tr>
                </thead>
                <tbody>
                  {rows.map((g) => {
                    const q = g.quadrant as ShownQuadrant;
                    const on = sel !== null && keyOf(sel) === keyOf(g);
                    return (
                      <tr key={g.tech}>
                        <th
                          scope="row"
                          className="py-0.5 pr-2 text-left font-normal"
                        >
                          <button
                            type="button"
                            className={`link text-left ${on ? "text-paper" : ""}`}
                            aria-pressed={on}
                            onClick={() => setChosen(keyOf(g))}
                            title={gapCellWords(g, focal, ctx.name)}
                          >
                            {g.tech}
                          </button>
                        </th>
                        <td className="pr-2">
                          <span
                            className="inline-flex h-5 w-5 items-center justify-center rounded-sm text-[0.65rem]"
                            style={cellStyle(q)}
                            title={QUADRANT_NAME[q]}
                          >
                            <span aria-hidden="true">{GAP_GLYPH[q]}</span>
                            <span className="sr-only">{QUADRANT_NAME[q]}</span>
                          </span>
                        </td>
                        <td>
                          <StageStrip
                            g={g}
                            label={`${g.tech}: by supply stage`}
                          />
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            ) : (
              <p className="mt-2 text-xs text-paper-mute">
                No complementarity or covered technology in this direction.
              </p>
            )}
          </div>
        ))}
      </div>
      <div className="mt-3 flex flex-wrap gap-x-6 gap-y-2">
        <GapLegend />
        <StageLegend />
      </div>
      {sel && (
        <p
          className="mt-3 rounded-md border border-ink-600 p-3 text-sm text-paper-dim"
          data-testid="pair-gap-detail"
          aria-live="polite"
        >
          <span className="text-paper">{QUADRANT_NAME[sel.quadrant]}</span> ·{" "}
          {gapText(sel, ctx)}
        </p>
      )}
    </div>
  );
}
