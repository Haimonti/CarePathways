import { AlertCircle } from "lucide-react";

export function ErrorBanner({ message }: { message: string }) {
  return (
    <div className="mb-4 flex w-full items-start gap-2.5 rounded-xl border border-ub-danger/30 bg-ub-dangerLight p-3.5 text-sm text-ub-danger">
      <AlertCircle className="mt-0.5 shrink-0" size={18} />
      <span>{message}</span>
    </div>
  );
}
