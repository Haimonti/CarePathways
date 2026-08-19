import { LlosBadge } from "./LlosBadge";

type PredictionCardProps = {
  predictedDays: number;
  isLlos: boolean;
  actualDays?: number | null;
  modelLabel?: string | null;
  statRows?: Record<string, string>;
};

export function PredictionCard({
  predictedDays,
  isLlos,
  actualDays,
  modelLabel,
  statRows = {},
}: PredictionCardProps) {
  const weeks = (predictedDays / 7).toFixed(1);
  const hours = Math.round(predictedDays * 24);

  return (
    <section className="rounded-[20px] bg-gradient-to-br from-ub-blue to-ub-blueDark p-7 text-white shadow-result">
      <div className="flex items-start justify-between gap-4">
        <div>
          <div className="mb-1 text-[11px] font-semibold uppercase tracking-[1.2px] text-white/70">
            Prediction Complete
          </div>
          <LlosBadge isLlos={isLlos} />
        </div>
        {modelLabel ? (
          <div className="max-w-[180px] truncate rounded-lg bg-white/10 px-2.5 py-1.5 text-[11px] font-medium">
            {modelLabel}
          </div>
        ) : null}
      </div>

      <div className="mt-6 flex items-end gap-2">
        <span className="text-[64px] font-extrabold leading-[0.9] tracking-[-2px]">
          {predictedDays.toFixed(1)}
        </span>
        <span className="pb-2 text-[22px] font-bold text-ub-gold">days</span>
      </div>
      <p className="mt-1 text-[13px] text-white/65">
        Predicted length of hospital stay
      </p>

      <div className="mt-6 grid grid-cols-2 overflow-hidden rounded-xl bg-white/10 min-[520px]:grid-cols-3">
        <StatCell label="Weeks" value={weeks} />
        <StatCell label="Hours" value={String(hours)} />
        {actualDays != null ? (
          <StatCell
            label="Actual LOS"
            value={`${actualDays.toFixed(1)} d`}
            highlight
          />
        ) : null}
      </div>

      {Object.keys(statRows).length > 0 ? (
        <div className="mt-4 space-y-1.5">
          {Object.entries(statRows).map(([label, value]) => (
            <div
              key={label}
              className="flex items-center justify-between gap-4 text-xs"
            >
              <span className="text-white/60">{label}</span>
              <span className="font-semibold text-white">{value}</span>
            </div>
          ))}
        </div>
      ) : null}
    </section>
  );
}

function StatCell({
  label,
  value,
  highlight = false,
}: {
  label: string;
  value: string;
  highlight?: boolean;
}) {
  return (
    <div className="border-r border-white/15 px-4 py-3.5 last:border-r-0">
      <div className="text-[10px] font-semibold uppercase tracking-[0.8px] text-white/55">
        {label}
      </div>
      <div
        className={[
          "mt-1 text-lg font-bold",
          highlight ? "text-ub-gold" : "text-white",
        ].join(" ")}
      >
        {value}
      </div>
    </div>
  );
}
