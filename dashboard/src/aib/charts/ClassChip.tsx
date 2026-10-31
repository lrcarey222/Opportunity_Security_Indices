// Class marks: a swatch (filled for the three intervention classes, hollow for LOW INTERVENTION, the
// no-data hatch for INSUFFICIENT DATA), a chip with the class name beside it, and the class legend. The
// name is always printed: colour is never the only carrier. Beside a node's class: its multi-label policy
// needs ("needs BUILD + MANAGE SPECIALIZATION") and, when the exporter writes it, the PHYSICAL CAPABILITY
// NOT VALIDATED warning as a visible tag (never only a tooltip); the same tag beside a class under a
// variant or a growth bound that carries it (FlaggedClass).

import { HATCH_STYLE, SIGNAL } from '../../theme/palette.ts';
import { AIB_CLASSES, AIB_PHYSICAL_WARNING, type AibNode } from '../contract.ts';
import { classInfo, needsAddToClass, policyNeedsWords } from '../derive/classes.ts';

export function ClassSwatch({ cls, size = 10 }: { cls: string; size?: number }) {
  const info = classInfo(cls);
  const style =
    info.mark === 'fill'
      ? { background: info.color }
      : info.mark === 'hollow'
        ? { border: `2px solid ${info.color}` }
        : { ...HATCH_STYLE, border: '1px solid #9ca3af' };
  return <span aria-hidden="true" className="inline-block shrink-0" style={{ width: size + 4, height: size, boxSizing: 'border-box', ...style }} />;
}

/** The chip's tooltip: what the class means, then the actions needed and the warning when there are any. */
export function classTitle(cls: string, needs?: readonly string[] | null, warning?: string | null): string {
  const t = policyNeedsWords(needs);
  return [classInfo(cls).meaning, t !== null ? `Actions needed: ${t}.` : null, warning ? `${warning}.` : null].filter(Boolean).join(' ');
}

/** The class as a tag: swatch and name. */
export function ClassChip({ cls, title, needs, warning }: { cls: string; title?: string; needs?: readonly string[] | null; warning?: string | null }) {
  const info = classInfo(cls);
  return (
    <span className="tag inline-flex items-center gap-1.5 whitespace-nowrap" title={title ?? classTitle(cls, needs, warning)}>
      <ClassSwatch cls={cls} />
      {info.label}
    </span>
  );
}

/**
 * The physical-capability warning as a visible tag (the warning text as the exporter writes it). Beside
 * the headline class its test id is "physical-warning"; beside a variant's or a growth bound's class,
 * "variant-physical-warning".
 */
export function PhysicalWarning({ warning, validation, testId = 'physical-warning' }: { warning: string; validation?: string | null; testId?: string }) {
  return (
    <span
      className="tag inline-flex items-center gap-1.5 whitespace-nowrap text-paper"
      style={{ borderColor: SIGNAL }}
      title={validation ? `Physical validation: ${validation}` : 'The class rests on trade data that physical production data do not confirm.'}
      data-testid={testId}
    >
      <span aria-hidden="true" style={{ color: SIGNAL }}>
        ▲
      </span>
      {warning}
    </span>
  );
}

/** A class under a variant or a growth bound by name, with the warning tag beside it when that class carries it. */
export function FlaggedClass({ cls, warning }: { cls: string | null; warning: boolean | null | undefined }) {
  if (cls === null) return <>–</>;
  return (
    <span className="inline-flex flex-wrap items-center gap-x-1.5 gap-y-1">
      {classInfo(cls).label}
      {warning === true && <PhysicalWarning warning={AIB_PHYSICAL_WARNING} testId="variant-physical-warning" />}
    </span>
  );
}

/** "Actions needed: Build capacity + Manage specialization" (`always`: also when the needs only repeat the class, or are none). */
export function PolicyNeeds({ cls, needs, always = false }: { cls: string; needs?: readonly string[] | null; always?: boolean }) {
  const t = policyNeedsWords(needs);
  if (t === null || (!always && !needsAddToClass(cls, needs))) return null;
  return (
    <span className="text-xs text-paper-dim" data-testid="policy-needs">
      <span className="text-paper-mute">Actions needed:</span> {t}
    </span>
  );
}

/** A node's class with its policy needs and its warning: everything a class reading needs beside it. */
export function NodeClass({ n, always = false }: { n: AibNode; always?: boolean }) {
  return (
    <span className="inline-flex flex-wrap items-center gap-x-2 gap-y-1">
      <ClassChip cls={n.cls} needs={n.policyNeeds} warning={n.warning} />
      <PolicyNeeds cls={n.cls} needs={n.policyNeeds} always={always} />
      {n.warning && <PhysicalWarning warning={n.warning} validation={n.physicalValidation} />}
    </span>
  );
}

/** The five classes with their marks (the legend of every class-coloured view). */
export function ClassLegend({ classes = AIB_CLASSES }: { classes?: readonly string[] }) {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="What each supply-chain step needs">
      {classes.map((c) => (
        <li key={c} className="inline-flex items-center gap-1.5">
          <ClassSwatch cls={c} />
          {classInfo(c).label}
        </li>
      ))}
    </ul>
  );
}
