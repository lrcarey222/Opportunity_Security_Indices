/** @type {import('tailwindcss').Config} */
export default {
  content: ["./index.html", "./src/**/*.{ts,tsx}"],
  theme: {
    extend: {
      colors: {
        base: "var(--color-base)",
        surface: "var(--color-surface)",
        panel: "var(--color-panel)",
        text: "var(--color-text)",
        muted: "var(--color-muted)",
        accent: "var(--color-accent)",
        "accent-strong": "var(--color-accent-strong)",
        border: "var(--color-border)",
      },
      boxShadow: {
        glow: "0 0 30px color-mix(in srgb, var(--color-accent) 45%, transparent)",
      },
    },
  },
  plugins: [],
};
