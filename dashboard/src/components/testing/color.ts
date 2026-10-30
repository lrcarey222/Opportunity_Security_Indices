// A test helper: the #rrggbb of a CSS colour as jsdom reports it ("rgb(245, 130, 41)" or "#f58229"; the
// first colour of a longer value such as a background shorthand). Null when the value names no colour.

export function cssHex(value: string | null | undefined): string | null {
  if (!value) return null;
  const hex = value.match(/#([0-9a-f]{6})\b/i);
  if (hex) return `#${hex[1].toLowerCase()}`;
  const rgb = value.match(/rgba?\(\s*(\d+)[,\s]+(\d+)[,\s]+(\d+)/i);
  if (!rgb) return null;
  return `#${rgb.slice(1, 4).map((n) => Number(n).toString(16).padStart(2, '0')).join('')}`;
}
