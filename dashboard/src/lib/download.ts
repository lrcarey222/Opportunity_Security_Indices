// Save a file from the browser (a Blob and a temporary object URL; no server).

export function downloadBlob(blob: Blob, filename: string): void {
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = filename;
  a.rel = 'noopener';
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

export function downloadText(text: string, filename: string, type = 'text/csv;charset=utf-8'): void {
  downloadBlob(new Blob([text], { type }), filename);
}

/** A file-name-safe slug. */
export function fileSlug(s: string): string {
  return s.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'table';
}
