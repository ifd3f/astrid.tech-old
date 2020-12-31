const legally = require("legally");

export async function findLicenses(): Promise<any> {
  console.log(await legally());
}

export async function getLicenseData() {
  console.log((await findLicenses())[0]);
}
