type StepIndicatorProps = {
  currentStep: number;
  totalSteps: number;
  labels: string[];
};

export function StepIndicator({
  currentStep,
  totalSteps,
  labels,
}: StepIndicatorProps) {
  const progress = totalSteps <= 1 ? 1 : currentStep / (totalSteps - 1);

  return (
    <div className="bg-white px-6 py-4">
      <div className="flex items-center gap-3">
        <div className="h-1 flex-1 overflow-hidden rounded-full bg-ub-surfaceVariant">
          <div
            className="h-full rounded-full bg-ub-blue transition-all duration-300"
            style={{ width: `${Math.round(progress * 100)}%` }}
          />
        </div>
        <span className="rounded-full border border-ub-blue/20 bg-ub-blue/10 px-2.5 py-1 text-[11px] font-bold text-ub-blue">
          {Math.round(progress * 100)}%
        </span>
      </div>
      <div className="mt-2 flex items-center justify-between text-xs font-medium">
        <span className="text-ub-textMuted">
          Step {currentStep + 1} of {totalSteps}
        </span>
        <span className="text-ub-textSecondary">{labels[currentStep]}</span>
      </div>
    </div>
  );
}
