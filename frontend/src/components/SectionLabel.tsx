export function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <div className="section-label">
      <span className="h-3.5 w-[3px] rounded-full bg-ub-blue" />
      <span>{children}</span>
    </div>
  );
}
