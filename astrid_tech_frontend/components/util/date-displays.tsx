import { format } from "date-fns";
import { FC } from "react";

export const DateInterval: FC<{
  formatStyle: string;
  startDate: Date;
  endDate?: Date | null;
}> = ({ formatStyle, startDate, endDate }) => {
  const startStr = (
    <time dateTime={startDate.toISOString()}>
      {format(startDate, formatStyle)}
    </time>
  );
  if (startDate == endDate) {
    return startStr;
  }

  const normEndDate = endDate ? endDate : new Date();
  const endStr = (
    <time dateTime={normEndDate.toISOString()}>
      {endDate ? format(normEndDate, formatStyle) : "now"}
    </time>
  );

  return (
    <>
      {startStr} to {endStr}
    </>
  );
};
