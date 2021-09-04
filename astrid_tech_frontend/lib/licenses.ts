import { promises as fs } from 'fs';

export type SPDX = {
  name: string;
  url: string;
  isInvalidSPDX?: boolean;
};

export type SoftwareLicenseLibrary = {
  license: string;
  name: string;
  url: string | null;
};

export type LicenseWithLibraries = {
  license: SPDX;
  libraries: SoftwareLicenseLibrary[];
};

export async function getLicenseData(): Promise<SoftwareLicenseLibrary[]> {
  const json = JSON.parse((await fs.readFile('data/licenses.json')).toString());

  return Object.keys(json).map((library) => {
    const entry = json[library];
    return {
      license: entry.licenses as string,
      name: library,
      url: entry.repository ?? null,
    };
  });
}
