type TextInputProps = {
  label: string;
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  icon?: React.ReactNode;
  rows?: number;
};

export function TextInput({
  label,
  value,
  onChange,
  placeholder,
  icon,
  rows,
}: TextInputProps) {
  const inputClass = icon ? "input pl-10" : "input";

  return (
    <label className="block">
      <span className="label">{label}</span>
      <span className="relative block">
        {icon ? (
          <span className="pointer-events-none absolute left-3 top-3.5 text-ub-textMuted">
            {icon}
          </span>
        ) : null}
        {rows ? (
          <textarea
            value={value}
            onChange={(event) => onChange(event.target.value)}
            rows={rows}
            placeholder={placeholder}
            className={inputClass}
          />
        ) : (
          <input
            value={value}
            onChange={(event) => onChange(event.target.value)}
            placeholder={placeholder}
            className={inputClass}
          />
        )}
      </span>
    </label>
  );
}
