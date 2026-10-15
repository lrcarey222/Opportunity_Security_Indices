// MemoDocument (spec_country.md A4.3, B7.2): renders the memo's structured document (the AST the PDF and
// the Word file are built from, so the three are word-for-word the same) as React elements: no HTML
// injection, no Markdown parser at runtime. Summary box with the short orange rule, numbered h2
// sections, superscript source numbers linking to the numbered source list. With `trace`, every value
// that came from the fact sheet is outlined and says its path and source in a tooltip.

import type { MemoDoc, MemoInline, MemoItem } from '../contract.ts';

function Inlines({ xs, trace, srcOf }: { xs: MemoInline[]; trace: boolean; srcOf: (n: number | null | undefined) => string | null }) {
  return (
    <>
      {xs.map((n, i) => {
        if (n.t === 'strong') return <strong key={i} className="font-medium text-paper">{n.v}</strong>;
        if (n.t === 'em') return <em key={i}>{n.v}</em>;
        if (n.t === 'value') {
          if (!trace) return <span key={i}>{n.v}</span>;
          const src = srcOf(n.src);
          return (
            <span key={i} className="border-b border-dotted border-signal" title={`${n.path}${src ? ` · ${src}` : ''}`} data-path={n.path}>
              {n.v}
            </span>
          );
        }
        return <span key={i}>{n.v}</span>;
      })}
    </>
  );
}

function Refs({ refs }: { refs: number[] }) {
  if (!refs.length) return null;
  return (
    <sup className="ml-0.5 text-[0.7em] text-paper-mute">
      {refs.map((r, i) => (
        <span key={r}>
          {i > 0 && ','}
          <a href={`#src-${r}`} className="link" aria-label={`Source ${r}`}>
            {r}
          </a>
        </span>
      ))}
    </sup>
  );
}

function Item({ it, trace, srcOf }: { it: MemoItem; trace: boolean; srcOf: (n: number | null | undefined) => string | null }) {
  return (
    <>
      <Inlines xs={it.inlines} trace={trace} srcOf={srcOf} />
      <Refs refs={it.refs} />
    </>
  );
}

export function MemoDocument({ doc, trace = false }: { doc: MemoDoc; trace?: boolean }) {
  const byN = new Map(doc.sources.map((s) => [s.n, s.text]));
  const srcOf = (n: number | null | undefined) => (n ? (byN.get(n) ?? null) : null);
  return (
    <article className="max-w-3xl text-base leading-relaxed text-paper-dim" aria-label={doc.title}>
      <section className="panel mb-8 p-5" aria-labelledby="memo-summary-h">
        <h2 id="memo-summary-h" className="rule-neis-sm mb-3 text-lg font-medium text-paper">
          Summary
        </h2>
        <ul className="list-disc space-y-2 pl-5">
          {doc.summary.map((it, i) => (
            <li key={i}>
              <Item it={it} trace={trace} srcOf={srcOf} />
            </li>
          ))}
        </ul>
      </section>
      {doc.sections.map((s) => (
        <section key={s.n} className="mb-8" aria-labelledby={`memo-s${s.n}`}>
          <h2 id={`memo-s${s.n}`} className="mb-3 text-xl font-medium text-paper">
            {s.n}. {s.title}
          </h2>
          {s.blocks.map((b, j) =>
            b.type === 'ul' ? (
              <ul key={j} className="mb-4 list-disc space-y-1.5 pl-5">
                {b.items.map((it, k) => (
                  <li key={k}>
                    <Item it={it} trace={trace} srcOf={srcOf} />
                  </li>
                ))}
              </ul>
            ) : (
              <p key={j} className="mb-4">
                <Item it={b} trace={trace} srcOf={srcOf} />
              </p>
            ),
          )}
        </section>
      ))}
      <section aria-labelledby="memo-sources-h" className="border-t hairline pt-5">
        <h2 id="memo-sources-h" className="mb-3 text-base font-medium text-paper">
          Sources
        </h2>
        <ol className="list-decimal space-y-1.5 pl-5 text-sm">
          {doc.sources.map((s) => (
            <li key={s.n} id={`src-${s.n}`} value={s.n}>
              {s.text}
            </li>
          ))}
        </ol>
      </section>
      <p className="mt-6 border-t hairline pt-3 text-sm italic text-paper-mute">
        {doc.label}
        {doc.footer && <span className="block not-italic">{doc.footer}</span>}
      </p>
    </article>
  );
}
