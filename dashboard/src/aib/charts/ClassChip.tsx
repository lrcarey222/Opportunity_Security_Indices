// Class marks: a swatch (filled for the three intervention classes, hollow for LOW INTERVENTION, the
// no-data hatch for INSUFFICIENT DATA), a chip with the class name beside it, and the class legend. The
// name is always printed: colour is never the only carrier.

import { HATCH_STYLE } from '../../theme/palette.ts';
import { AIB_CLASSES } from '../contract.ts';
import { classInfo } from '../derive/classes.ts';

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

/** The class as a tag: swatch and name. */
export function ClassChip({ cls, title }: { cls: string; title?: string }) {
  const info = classInfo(cls);
  return (
    <span className="tag inline-flex items-center gap-1.5 whitespace-nowrap" title={title ?? info.meaning}>
      <ClassSwatch cls={cls} />
      {info.label}
    </span>
  );
}

/** The five classes with their marks (the legend of every class-coloured view). */
export function ClassLegend({ classes = AIB_CLASSES }: { classes?: readonly string[] }) {
  return (
    <ul className="flex flex-wrap gap-x-4 gap-y-1.5 text-xs text-paper-dim" aria-label="Node classes">
      {classes.map((c) => (
        <li key={c} className="inline-flex items-center gap-1.5">
          <ClassSwatch cls={c} />
          {classInfo(c).label}
        </li>
      ))}
    </ul>
  );
}
