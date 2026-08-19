"use client";

import Link from "next/link";
import { ChevronLeft, Hospital } from "lucide-react";

type UbHeaderProps = {
  title: string;
  subtitle?: string;
  showBack?: boolean;
  onReset?: () => void;
};

export function UbHeader({
  title,
  subtitle,
  showBack = false,
  onReset,
}: UbHeaderProps) {
  return (
    <header className="bg-ub-blue text-white">
      <div className="mx-auto flex min-h-16 max-w-[760px] items-center gap-3 px-4 sm:px-5">
        {showBack ? (
          <Link
            href="/"
            aria-label="Back to home"
            className="inline-flex h-10 w-10 items-center justify-center rounded-lg text-white transition hover:bg-white/10"
          >
            <ChevronLeft size={22} />
          </Link>
        ) : null}

        <div className="flex min-w-0 flex-1 items-center gap-2.5">
          <div className="flex h-7 w-7 shrink-0 items-center justify-center rounded-md bg-white/15">
            <Hospital size={16} />
          </div>
          <div className="min-w-0">
            <div className="truncate text-[17px] font-bold tracking-[-0.3px]">
              {title}
            </div>
            {subtitle ? (
              <div className="truncate text-[11px] font-normal text-white/70">
                {subtitle}
              </div>
            ) : null}
          </div>
        </div>

        {onReset ? (
          <button
            type="button"
            onClick={onReset}
            className="rounded-md px-2 py-1 text-[13px] text-white/70 transition hover:bg-white/10 hover:text-white"
          >
            Reset
          </button>
        ) : null}

        <div className="rounded-md bg-ub-gold px-2.5 py-1 text-xs font-black tracking-[0.5px] text-ub-blueDark">
          UB
        </div>
      </div>
    </header>
  );
}
