type LlosBadgeProps = {
  isLlos: boolean;
  small?: boolean;
};

export function LlosBadge({ isLlos, small = false }: LlosBadgeProps) {
  return (
    <span
      className={[
        "inline-flex items-center gap-1.5 rounded-full border px-3 py-1 font-bold tracking-[0.2px]",
        small ? "text-[11px]" : "text-xs",
        isLlos
          ? "border-ub-gold/40 bg-ub-goldWash text-ub-goldDark"
          : "border-ub-success/40 bg-ub-successLight text-ub-success",
      ].join(" ")}
    >
      <span
        className={[
          "h-1.5 w-1.5 rounded-full",
          isLlos ? "bg-ub-gold" : "bg-ub-success",
        ].join(" ")}
      />
      {isLlos ? "LLOS — Long Stay" : "Normal Stay"}
    </span>
  );
}
