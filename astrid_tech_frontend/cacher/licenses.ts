const crawler = require('npm-license-crawler');

export function generateLicenses(reportPath: string) {
  console.log('Generating license report');

  const options = {
    json: reportPath,
    start: ['.'],
  };

  return new Promise<any>((resolve, reject) =>
    crawler.dumpLicenses(options, function (error: any, res: any) {
      if (error) {
        reject(error);
      } else {
        resolve(res);
      }
    })
  );
}
