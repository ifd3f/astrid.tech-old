import crypto from "crypto";
import { format } from "date-fns";
import hslToHex from "hsl-to-hex";
import seedrandom from "seedrandom";
import { TimeSlug } from "./db/TimeSlug";

export function getContrastingTextColor(backgroundColor: string): string {
  const [, r, g, b] = backgroundColor
    .match(/#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i)!
    .map((x) => new Number("0x" + x) as number);
  return r * 0.299 + g * 0.587 + b * 0.114 > 140 ? "#000000" : "#ffffff";
}

var id = 0;
export function getUniqueId() {
  return id++;
}

export function hashString(str: string) {
  var hash = 0,
    chr,
    i;
  for (i = 0; i < str.length; i++) {
    chr = str.charCodeAt(i);
    hash = (hash << 5) - hash + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}

export function groupBy<T>(xs: T[], key: (x: T) => string) {
  const out = new Map<string, T[]>();
  for (const x of xs) {
    const k = key(x);
    out.get(k)?.push(x) ?? out.set(k, [x]);
  }
  return out;
}

export type PersistentColorTheme = {
  h: [number, number];
  s: [number, number];
  v: [number, number];
};

export function rescale(x: number, [a, b]: [number, number]) {
  return (b - a) * x + a;
}

export const PastelTheme: PersistentColorTheme = {
  h: [0, 360],
  s: [50, 50],
  v: [80, 80],
};

export const RichColorTheme: PersistentColorTheme = {
  h: [0, 360],
  s: [100, 100],
  v: [30, 60],
};

export function getPersistentColor(
  slug: string,
  theme: PersistentColorTheme = PastelTheme
): [number, number, number] {
  const random = seedrandom(
    crypto.createHash(`md5`).update(slug).digest(`hex`)
  );
  var h = (rescale(random(), theme.h) | 0) % 360;
  var s = rescale(random(), theme.s) | 0;
  var v = rescale(random(), theme.v) | 0;
  return [h, s, v];
}

export function getHSLString([h, s, l]: number[]) {
  return hslToHex(h, s, l) as string;
}

export function formatDateInterval(
  formatStyle: string,
  startDate: Date,
  endDate?: Date | null
) {
  const startStr = format(startDate, formatStyle);
  if (startDate == endDate) {
    return startStr;
  }
  return endDate
    ? `${startStr} to ${format(endDate, formatStyle)}`
    : `${startStr} to now`;
}

export function truncateKeepWords(text: string, maxChars: number) {
  if (text.length < maxChars) return "";

  for (let cutoff = maxChars; cutoff > 0; cutoff--) {
    if (text.charAt(cutoff) == " ") {
      return text.substr(0, cutoff);
    }
  }
  return "";
}
