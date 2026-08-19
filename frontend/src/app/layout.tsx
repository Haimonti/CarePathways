import type { Metadata } from "next";
import "./globals.css";

export const metadata: Metadata = {
  title: "LLOS — Length of Stay Predictor",
  description: "University at Buffalo research tool for clinical LOS prediction.",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className="font-sans antialiased">{children}</body>
    </html>
  );
}
