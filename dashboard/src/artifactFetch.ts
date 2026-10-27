// The beta preview hosted as a claude.ai artifact (VITE_ARTIFACT=1) can serve only standard web file
// types, so each .osb chunk ships as base64 text next to where it would be (<file>.osb.txt). Imported
// first by main.tsx and by the engine worker: there, fetch() of a .osb URL reads the text and answers
// with the original bytes. A no-op in every other build.

if (import.meta.env.VITE_ARTIFACT === '1') {
  const realFetch = globalThis.fetch.bind(globalThis);
  globalThis.fetch = async (input: RequestInfo | URL, init?: RequestInit): Promise<Response> => {
    const url = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
    if (!/\.osb(?=[?#]|$)/.test(url)) return realFetch(input, init);
    const res = await realFetch(url.replace(/\.osb(?=[?#]|$)/, '.osb.txt'), init);
    if (!res.ok) return res;
    const bin = atob((await res.text()).trim());
    const bytes = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
    return new Response(bytes, { status: 200, headers: { 'Content-Type': 'application/octet-stream' } });
  };
}

export {};
