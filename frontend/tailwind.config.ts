import type { Config } from "tailwindcss";

const config: Config = {
  content: ["./src/**/*.{js,ts,jsx,tsx,mdx}"],
  theme: {
    extend: {
      colors: {
        ub: {
          blue: "#005BBB",
          blueDark: "#003D82",
          blueLight: "#1A72CC",
          gold: "#F0AB00",
          goldDark: "#CC9100",
          goldLight: "#FFC533",
          background: "#F5F7FB",
          surface: "#FFFFFF",
          surfaceVariant: "#EEF2FA",
          border: "#DDE3EF",
          textPrimary: "#0D1A33",
          textSecondary: "#4A5568",
          textMuted: "#8A9BB2",
          success: "#16A34A",
          successLight: "#F0FDF4",
          danger: "#DC2626",
          dangerLight: "#FEF2F2",
          goldWash: "#FFFBEB"
        },
      },
      boxShadow: {
        result: "0 8px 20px rgba(0, 91, 187, 0.3)",
      },
      fontFamily: {
        sans: ["Inter", "system-ui", "sans-serif"],
      },
    },
  },
  plugins: [],
};

export default config;
