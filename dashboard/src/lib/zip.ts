// A zip of text files built in the browser with fflate (loaded only by the Data view).

import { strToU8, zipSync } from 'fflate';

export function zipTexts(files: Record<string, string>): Uint8Array {
  const entries: Record<string, Uint8Array> = {};
  for (const [name, text] of Object.entries(files)) entries[name] = strToU8(text);
  return zipSync(entries, { level: 6 });
}
