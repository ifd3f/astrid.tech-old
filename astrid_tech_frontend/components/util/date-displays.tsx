import classNames from "classnames";
import { format } from "date-fns";
import { FC } from "react";

export type DateIntervalProps = {
  formatStyle: string;
  startDate: Date;
  endDate?: Date | null;
};

export const DateInterval: FC<DateIntervalProps> = ({
  formatStyle,
  startDate,
  endDate,
}) => {
  const startStr = <SemanticDate date={startDate} formatStyle={formatStyle} />;
  if (startDate == endDate) {
    return startStr;
  }

  const endStr = (
    <SemanticDate date={endDate ? endDate : "now"} formatStyle={formatStyle} />
  );

  return (
    <>
      {startStr} to {endStr}
    </>
  );
};

type SemanticDateProps = {
  className?: string;
  formatStyle: string;
  date: Date | "now";
};

export const SemanticDate: FC<SemanticDateProps> = ({
  formatStyle,
  className,
  date,
}) => {
  return date == "now" ? (
    <time className={className} dateTime={new Date().toISOString()}>
      now
    </time>
  ) : (
    <time className={className} dateTime={date.toISOString()}>
      {format(date, formatStyle)}
    </time>
  );
};
