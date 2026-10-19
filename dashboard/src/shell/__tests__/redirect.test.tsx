// <Redirect> (shell/Redirect.tsx): a redirect with a notice to a lazy route lands once. An inline
// `<Navigate state={{ notice }}>` re-navigates on every render while the target is loading and loops.

import { afterAll, afterEach, beforeAll, describe, expect, it, vi } from 'vitest';
import { act, cleanup, render } from '@testing-library/react';
import { createMemoryRouter, RouterProvider, useLocation, useNavigation } from 'react-router';
import { Redirect } from '../Redirect.tsx';

// the data router builds a fetch Request with jsdom's AbortSignal, which Node's Request rejects
const NodeRequest = globalThis.Request;
beforeAll(() => {
  globalThis.Request = class extends NodeRequest {
    constructor(input: RequestInfo | URL, init?: RequestInit) {
      const rest = { ...(init ?? {}) };
      delete (rest as { signal?: unknown }).signal;
      super(input, rest);
    }
  } as typeof Request;
});
afterAll(() => {
  globalThis.Request = NodeRequest;
});
afterEach(() => cleanup());

/** A page that re-renders while a navigation is pending (as the module pages do) and redirects. */
function From() {
  useNavigation();
  return <Redirect to="/to#here" notice="No such page; showing this one." />;
}

function Target() {
  const loc = useLocation();
  return <p>target: {(loc.state as { notice?: string } | null)?.notice}</p>;
}

describe('Redirect', () => {
  it('reaches a lazy route once, with the notice in the location state', async () => {
    const errors = vi.spyOn(console, 'error').mockImplementation(() => undefined);
    let loads = 0;
    const router = createMemoryRouter(
      [
        { path: '/from', Component: From },
        {
          path: '/to',
          lazy: async () => {
            loads += 1;
            await new Promise((r) => setTimeout(r, 200));
            return { Component: Target };
          },
        },
      ],
      { initialEntries: ['/from'] },
    );
    const navigations: string[] = [];
    router.subscribe((s) => {
      if (s.navigation.state === 'loading') navigations.push(s.navigation.location?.pathname ?? '');
    });
    render(<RouterProvider router={router} />);
    await act(async () => {
      await new Promise((r) => setTimeout(r, 600));
    });
    expect(router.state.location.pathname + router.state.location.hash).toBe('/to#here');
    expect((router.state.location.state as { notice?: string }).notice).toBe('No such page; showing this one.');
    expect(navigations.length).toBeLessThanOrEqual(2);
    expect(loads).toBe(1);
    expect(errors.mock.calls.filter((c) => /Maximum update depth/.test(String(c[0])))).toEqual([]);
    errors.mockRestore();
  });
});
