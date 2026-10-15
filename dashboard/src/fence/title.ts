// Document titles for Draw the Fence: "Result | Draw the Fence | NEIS Center".

import { useEffect } from 'react';

export const FENCE_TITLE = 'Draw the Fence | NEIS Center';

export function pageTitle(view: string): string {
  return view ? `${view} | ${FENCE_TITLE}` : FENCE_TITLE;
}

export function useTitle(view: string): void {
  useEffect(() => {
    document.title = pageTitle(view);
  }, [view]);
}
