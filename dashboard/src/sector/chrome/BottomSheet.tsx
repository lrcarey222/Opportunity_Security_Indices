// BottomSheet (spec_sector.md 4.0, 6, 7): a modal dialog that slides up from the bottom on phones (the
// SectorBar's filters). Same pattern as the Strategic DialDrawer: role="dialog", aria-modal, focus
// moves in on open and back to the opener on close, Escape closes, Tab is trapped inside, a click on
// the backdrop closes.

import { useEffect, useId, useRef, type ReactNode } from 'react';

const FOCUSABLE = 'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), summary, [tabindex]:not([tabindex="-1"])';

export interface BottomSheetProps {
  open: boolean;
  onClose: () => void;
  title: string;
  children: ReactNode;
  /** the close button's text (default "Done") */
  doneLabel?: string;
}

export function BottomSheet({ open, onClose, title, children, doneLabel = 'Done' }: BottomSheetProps) {
  const ref = useRef<HTMLDivElement>(null);
  const titleId = useId();
  const closeRef = useRef(onClose);
  closeRef.current = onClose;

  useEffect(() => {
    if (!open) return;
    const prev = document.activeElement as HTMLElement | null;
    const panel = ref.current;
    panel?.focus();
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        e.preventDefault();
        closeRef.current();
        return;
      }
      if (e.key !== 'Tab' || !panel) return;
      const items = [...panel.querySelectorAll<HTMLElement>(FOCUSABLE)].filter((el) => !el.hasAttribute('disabled'));
      if (!items.length) {
        e.preventDefault();
        panel.focus();
        return;
      }
      const first = items[0];
      const last = items[items.length - 1];
      const active = document.activeElement;
      if (e.shiftKey && (active === first || active === panel)) {
        e.preventDefault();
        last.focus();
      } else if (!e.shiftKey && active === last) {
        e.preventDefault();
        first.focus();
      } else if (!panel.contains(active)) {
        e.preventDefault();
        first.focus();
      }
    };
    document.addEventListener('keydown', onKey);
    return () => {
      document.removeEventListener('keydown', onKey);
      prev?.focus?.();
    };
  }, [open]);

  if (!open) return null;
  return (
    <div className="fixed inset-0 z-50 flex flex-col justify-end" role="dialog" aria-modal="true" aria-labelledby={titleId}>
      <div className="absolute inset-0 bg-black/60" onClick={onClose} aria-hidden="true" data-testid="sheet-backdrop" />
      <div ref={ref} tabIndex={-1} className="relative max-h-[85vh] overflow-y-auto border-t hairline bg-ink-800 px-4 pb-6 pt-4 outline-none">
        <div className="mb-4 flex items-center justify-between gap-4">
          <h2 id={titleId} className="text-base font-medium text-paper">
            {title}
          </h2>
          <button type="button" className="btn btn-ghost btn-sm" onClick={onClose}>
            {doneLabel}
          </button>
        </div>
        {children}
      </div>
    </div>
  );
}
