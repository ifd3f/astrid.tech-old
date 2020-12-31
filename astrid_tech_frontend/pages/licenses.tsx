import { InferGetStaticPropsType } from "next";
import React, { FC } from "react";
import { BsCircleFill } from "react-icons/bs";
import { PieChart } from "react-minimal-pie-chart";
import { Container } from "reactstrap";
import spdxLicenseList from "spdx-license-list";
import Layout from "../components/layout/layout";
import SEO from "../components/seo";
import {
  getLicenseData,
  LicenseWithLibraries,
  SoftwareLicenseLibrary,
} from "../lib/licenses";
import { getHSLString, getPersistentColor, RichColorTheme } from "../lib/util";

const LicenseSection: FC<LicenseWithLibraries> = ({ libraries, license }) => (
  <section>
    <h2>
      {license.url ? <a href={license.url}>{license.name}</a> : license.name}
    </h2>
    <p>{libraries.length} dependencies use this license.</p>
    <ul>
      {libraries.map((library) => (
        <li>
          {library.url ? (
            <a href={library.url}>{library.name}</a>
          ) : (
            library.name
          )}
        </li>
      ))}
    </ul>
  </section>
);

type LicensesChartProps = {
  licenses: LicenseWithLibraries[];
};

const LicensesChart: FC<LicensesChartProps> = ({ licenses }) => {
  const validLicenses = licenses.filter(
    ({ license }) => !license.isInvalidSPDX
  );
  const invalidLicenses = licenses.filter(
    ({ license }) => license.isInvalidSPDX
  );

  const other = { title: "Other", value: 0, color: "#888888", url: null };
  invalidLicenses.forEach(({ libraries }) => {
    other.value += libraries.length;
  });

  const data = validLicenses.map(({ license, libraries }) => ({
    title: license.name,
    value: libraries.length,
    color: getHSLString(getPersistentColor(license.name, RichColorTheme)),
    url: license.url as string | null,
  }));
  data.push(other);

  return (
    <section>
      <PieChart data={data} />
      <div>
        <h3>Legend</h3>
        <ul>
          {data.map(({ title, color, value, url }) => (
            <li>
              <BsCircleFill style={{ color }} />{" "}
              {url ? <a href={url}>{title}</a> : title} ({value})
            </li>
          ))}
        </ul>
      </div>
    </section>
  );
};

export async function getStaticProps() {
  const libraries = await getLicenseData();
  const licenseUsageCounts = new Map<string, SoftwareLicenseLibrary[]>();

  for (const library of libraries) {
    if (!licenseUsageCounts.has(library.license)) {
      licenseUsageCounts.set(library.license, []);
    }
    licenseUsageCounts.get(library.license)!!.push(library);
  }

  const licenses = [...licenseUsageCounts.keys()].map((name) => ({
    license: spdxLicenseList[name] ?? { name, isInvalidSPDX: true },
    libraries: licenseUsageCounts.get(name)!!,
  }));
  console.log(licenses[0]);

  return {
    props: {
      licenses,
      libraryCount: libraries.length,
    },
  };
}

const Licenses: FC<InferGetStaticPropsType<typeof getStaticProps>> = ({
  licenses,
  libraryCount,
}) => {
  const thisLicense = spdxLicenseList["AGPL-3.0"];

  return (
    <Layout currentLocation="about">
      <SEO
        title="Open-Source Licenses"
        description="Legal crap for the libraries my website uses"
      />
      <Container>
        <h1>Open-Source Licenses</h1>
        <p>
          astrid.tech is licensed under{" "}
          <a href={thisLicense.url}>{thisLicense.name}</a>. It also uses{" "}
          {libraryCount} NPM dependencies, which use {licenses.length} unique
          licensing configurations.
        </p>
        <LicensesChart licenses={licenses} />
        {licenses.map((licenses) => (
          <LicenseSection {...licenses} />
        ))}
      </Container>
    </Layout>
  );
};

export default Licenses;
