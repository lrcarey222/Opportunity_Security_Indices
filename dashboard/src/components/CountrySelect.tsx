// Searchable economy picker (spec_react.md 8.8): an input with a listbox (ARIA combobox pattern),
// keyboard-navigable (arrows, Enter, Escape), grouped by coalition tier.

import { useEffect, useId, useMemo, useRef, useState } from 'react';

export interface CountryOption {
  iso3c: string;
  country: string;
  group: string;
}

export function CountrySelect({
  label, value, options, onChange, className,
}: {
  label: string;
  value: string;
  options: CountryOption[];
  onChange: (iso3c: string) => void;
  className?: string;
}) {
  const id = useId();
  const listId = `${id}-list`;
  const current = options.find((o) => o.iso3c === value);
  const [query, setQuery] = useState('');
  const [open, setOpen] = useState(false);
  const [active, setActive] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const listRef = useRef<HTMLUListElement>(null);

  const matches = useMemo(() => {
    const q = query.trim().toLowerCase();
    const xs = q ? options.filter((o) => o.country.toLowerCase().includes(q) || o.iso3c.toLowerCase() === q) : options;
    return xs.slice(0, 300);
  }, [options, query]);

  useEffect(() => setActive(0), [query]);
  useEffect(() => {
    if (!open) return;
    const el = listRef.current?.querySelector<HTMLElement>(`[data-index="${active}"]`);
    el?.scrollIntoView?.({ block: 'nearest' });
  }, [active, open]);

  const choose = (o: CountryOption | undefined) => {
    if (!o) return;
    onChange(o.iso3c);
    setQuery('');
    setOpen(false);
  };

  let lastGroup = '';
  return (
    <div className={`relative ${className ?? 'w-64'}`}>
      <label htmlFor={id} className="label">
        {label}
      </label>
      <input
        ref={inputRef}
        id={id}
        className="field field-sm"
        role="combobox"
        aria-expanded={open}
        aria-controls={listId}
        aria-autocomplete="list"
        aria-activedescendant={open && matches[active] ? `${id}-o-${matches[active].iso3c}` : undefined}
        autoComplete="off"
        placeholder={current?.country ?? 'Search economies'}
        value={open ? query : (current?.country ?? '')}
        onFocus={() => {
          setOpen(true);
          setQuery('');
        }}
        onBlur={() => setTimeout(() => setOpen(false), 150)}
        onChange={(e) => {
          setQuery(e.target.value);
          setOpen(true);
        }}
        onKeyDown={(e) => {
          if (e.key === 'ArrowDown') {
            e.preventDefault();
            setOpen(true);
            setActive((a) => Math.min(matches.length - 1, a + 1));
          } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            setActive((a) => Math.max(0, a - 1));
          } else if (e.key === 'Enter') {
            if (open) {
              e.preventDefault();
              choose(matches[active]);
              inputRef.current?.blur();
            }
          } else if (e.key === 'Escape') {
            setOpen(false);
            setQuery('');
          }
        }}
      />
      {open && (
        <ul
          ref={listRef}
          id={listId}
          role="listbox"
          aria-label={label}
          className="absolute z-30 mt-1 max-h-72 w-full overflow-auto border border-ink-600 bg-ink-900 py-1 text-sm"
        >
          {matches.length === 0 && <li className="px-3 py-2 text-paper-mute">No economy matches</li>}
          {matches.map((o, i) => {
            const head = o.group !== lastGroup ? o.group : null;
            lastGroup = o.group;
            return (
              <li key={o.iso3c} role="presentation">
                {head && <div className="eyebrow px-3 pb-1 pt-2">{head}</div>}
                <div
                  id={`${id}-o-${o.iso3c}`}
                  role="option"
                  data-index={i}
                  aria-selected={o.iso3c === value}
                  className={`cursor-pointer px-3 py-1.5 ${i === active ? 'bg-ink-700 text-paper' : 'text-paper-dim'} ${o.iso3c === value ? 'font-medium' : ''}`}
                  onMouseDown={(e) => {
                    e.preventDefault();
                    choose(o);
                  }}
                  onMouseEnter={() => setActive(i)}
                >
                  {o.country} <span className="text-paper-mute">{o.iso3c}</span>
                </div>
              </li>
            );
          })}
        </ul>
      )}
    </div>
  );
}
