import Link from "next/link";
import {
  ArrowRight,
  ClipboardList,
  Hospital,
  Search,
} from "lucide-react";

export default function HomePage() {
  return (
    <main className="min-h-screen bg-ub-background">
      <div className="mx-auto max-w-[680px] px-6 py-10">
        <header>
          <div className="flex items-center gap-3.5">
            <div className="flex h-12 w-12 items-center justify-center rounded-xl bg-ub-blue text-white">
              <Hospital size={26} />
            </div>
            <div>
              <div className="flex items-center gap-2">
                <h1 className="text-[22px] font-black tracking-[-0.5px] text-ub-blue">
                  LLOS
                </h1>
                <span className="rounded bg-ub-gold px-2 py-0.5 text-[10px] font-black tracking-[0.5px] text-ub-blueDark">
                  UB
                </span>
              </div>
              <p className="text-xs font-medium text-ub-textSecondary">
                Long Length of Stay Predictor
              </p>
            </div>
          </div>

          <div className="mt-7">
            <h2 className="whitespace-pre-line text-4xl font-bold leading-[1.1] tracking-[-1px] text-ub-textPrimary">
              {"Predict hospital\nlength of stay."}
            </h2>
            <p className="mt-2.5 text-[15px] leading-6 text-ub-textSecondary">
              University at Buffalo research tool for clinical LOS prediction
              using the T5 + BiLSTM + LightGBM pipeline.
            </p>
          </div>
        </header>

        <section className="mt-12">
          <p className="mb-4 text-[11px] font-semibold uppercase tracking-[1.5px] text-ub-textMuted">
            Select Mode
          </p>
          <div className="space-y-4">
            <ModeCard
              href="/manual-entry"
              icon={<ClipboardList size={22} />}
              iconColor="text-ub-blue"
              iconBg="bg-ub-blue/10"
              badge="Clinical Form"
              badgeColor="text-ub-blue"
              title="Manual Entry"
              description="Enter patient clinical data across 5 steps — chief complaint, HPI, vitals, labs, conditions, and medications — then run LOS prediction."
              ctaLabel="Start Clinical Form"
              ctaClass="bg-ub-blue hover:bg-ub-blueDark"
            />
            <ModeCard
              href="/records"
              icon={<Search size={22} />}
              iconColor="text-ub-goldDark"
              iconBg="bg-ub-gold/10"
              badge="EHR Lookup"
              badgeColor="text-ub-goldDark"
              title="Patient Record Lookup"
              description="Search Dataset.csv by UUID, subject ID, or admission ID. Select a model, then get an instant LOS prediction with actual LOS when available."
              ctaLabel="Search Records"
              ctaClass="bg-ub-goldDark hover:bg-[#a87500]"
            />
          </div>
        </section>

        <footer className="mt-12 text-center text-[11px] text-ub-textMuted">
          LLOS threshold: &gt; 14 days · Synthea Synthetic EHR · UB Research
          Preview
        </footer>
      </div>
    </main>
  );
}

function ModeCard({
  href,
  icon,
  iconColor,
  iconBg,
  badge,
  badgeColor,
  title,
  description,
  ctaLabel,
  ctaClass,
}: {
  href: string;
  icon: React.ReactNode;
  iconColor: string;
  iconBg: string;
  badge: string;
  badgeColor: string;
  title: string;
  description: string;
  ctaLabel: string;
  ctaClass: string;
}) {
  return (
    <Link href={href} className="card block p-6 transition hover:border-ub-blue/30">
      <div className="flex items-center gap-3">
        <div
          className={`flex h-11 w-11 items-center justify-center rounded-[10px] ${iconBg} ${iconColor}`}
        >
          {icon}
        </div>
        <span
          className={`rounded-full border border-current/20 bg-current/5 px-2.5 py-1 text-[11px] font-bold tracking-[0.3px] ${badgeColor}`}
        >
          {badge}
        </span>
        <ArrowRight className="ml-auto text-ub-textMuted" size={16} />
      </div>
      <h3 className="mt-4 text-lg font-semibold text-ub-textPrimary">{title}</h3>
      <p className="mt-1.5 text-[13px] leading-5 text-ub-textSecondary">
        {description}
      </p>
      <span
        className={`mt-5 inline-flex min-h-[52px] w-full items-center justify-center rounded-xl px-7 py-3.5 text-sm font-semibold text-white transition ${ctaClass}`}
      >
        {ctaLabel}
      </span>
    </Link>
  );
}
