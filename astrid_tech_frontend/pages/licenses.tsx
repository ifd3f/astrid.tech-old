import { InferGetStaticPropsType } from 'next';
import React, { FC } from 'react';
import { BsCircleFill } from 'react-icons/bs';
import Masonry from 'react-masonry-component';
import { PieChart } from 'react-minimal-pie-chart';
import { Col, Container, Row } from 'reactstrap';
import spdxLicenseList from 'spdx-license-list';
import Layout from '../components/layout/layout';
import SEO from '../components/seo';
import {
  getLicenseData,
  LicenseWithLibraries,
  SoftwareLicenseLibrary,
} from '../lib/licenses';
import { getHSLString, getPersistentColor, RichColorTheme } from '../lib/util';

const LicenseSection: FC<LicenseWithLibraries> = ({ libraries, license }) => (
  <section>
    <h2 id={license.name}>
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

  const other = { title: 'Other', value: 0, color: '#888888', url: null };
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
    <Row tag="section">
      <Col xs="12" lg="6">
        <picture style={{ display: 'flex', justifyContent: 'space-around' }}>
          <PieChart data={data} style={{ maxWidth: 600, maxHeight: 600 }} />
        </picture>
      </Col>
      <Col>
        <h3>Legend</h3>
        <ul>
          {data.map(({ title, color, value, url }) =>
            url ? (
              <li>
                <BsCircleFill style={{ color }} /> {title} (
                <a href={`#${title}`}>{value} dependencies</a> |{' '}
                <a href={url}>more info</a>)
              </li>
            ) : (
              <li>
                <BsCircleFill style={{ color }} /> {title} ({value}{' '}
                dependencies)
              </li>
            )
          )}
        </ul>
      </Col>
    </Row>
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
  const thisLicense = spdxLicenseList['AGPL-3.0'];

  return (
    <Layout currentLocation="about">
      <SEO
        title="Open-Source Licenses"
        description="Legal crap for the libraries my website uses"
      />
      <Container>
        <article>
          <h1>Open-Source Licenses</h1>
          <p>
            astrid.tech is licensed under{' '}
            <strong>
              <a href={thisLicense.url}>{thisLicense.name}</a>
            </strong>
            . It uses <strong>{libraryCount}</strong> NPM dependencies, which
            use <strong>{licenses.length}</strong> unique licensing
            configurations.
          </p>
          <LicensesChart licenses={licenses} />
          <Masonry options={{ transitionDuration: 0 }}>
            {licenses.map((licenses) => (
              <Col sm="6">
                <LicenseSection {...licenses} />
              </Col>
            ))}
          </Masonry>
        </article>
      </Container>
    </Layout>
  );
};

export default Licenses;
