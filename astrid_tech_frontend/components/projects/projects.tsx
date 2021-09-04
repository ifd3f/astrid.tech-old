import Fuse from 'fuse.js';
import React, { ChangeEventHandler, FC, useContext, useState } from 'react';
import { BsCaretDown, BsCaretUp, BsX } from 'react-icons/bs';
import { FaInfoCircle } from 'react-icons/fa';
import {
  Badge,
  Button,
  Card,
  CardBody,
  Col,
  Collapse,
  Container,
  Input,
  InputGroup,
  Row,
  UncontrolledTooltip,
} from 'reactstrap';
import { Project, ProjectMeta, Tag } from '../../types/types';
import Layout, { PageHeading } from '../layout';
import SEO from '../seo';
import { TagBadge } from '../tags/tag';
import { useTagTable } from '../tags/TagTableProvider';
import { ProjectCard } from './project-card';
import styles from './projects.module.scss';
import { Filterer, SearchContext } from './search';

type CountBadgeProps = {
  tag: string | Tag;
  count: number;
};

const CountBadge: FC<CountBadgeProps> = ({ tag, count }) => {
  const table = useTagTable();
  const data = table.get(tag);
  return (
    <Badge
      style={{
        color: data.backgroundColor,
        backgroundColor: data.color,
      }}
    >
      {count}
    </Badge>
  );
};

const SelectableTagList: FC = () => {
  const { selectableTags, tagUsageCounts, addFilterTag } =
    useContext(SearchContext);

  return (
    <div className={styles.selectableTagsContainer}>
      {selectableTags.map((tag) => (
        <span
          className={styles.selectableTag}
          onClick={() => addFilterTag(tag)}
          key={tag}
        >
          <TagBadge tag={tag}>
            {' '}
            <CountBadge tag={tag} count={tagUsageCounts.get(tag)!} />
          </TagBadge>
        </span>
      ))}
    </div>
  );
};

const CurrentlyUsedTagList: FC = () => {
  const { filterTags, tagUsageCounts, removeFilterTag } =
    useContext(SearchContext);
  return (
    <div>
      {[...filterTags.values()].map((tag) => (
        <span
          className={styles.deletableTag}
          onClick={() => removeFilterTag(tag)}
          key={tag}
        >
          <TagBadge tag={tag}>
            {' '}
            <CountBadge tag={tag} count={tagUsageCounts.get(tag)!} />
            <BsX />
          </TagBadge>
        </span>
      ))}
    </div>
  );
};

const TagsFilterDropdown: FC = () => {
  const [tagListOpen, setTagListOpen] = useState(false);
  const toggleOpen = () => {
    setTagListOpen(!tagListOpen);
  };

  return (
    <Card>
      <CardBody>
        <Row>
          <h3
            style={{
              paddingLeft: 10,
            }}
          >
            Filter by tag
          </h3>
          <Col style={{ textAlign: 'right' }}>
            <Button onClick={toggleOpen} outline size="sm">
              {tagListOpen ? <BsCaretUp /> : <BsCaretDown />}
            </Button>
          </Col>
        </Row>
        <Collapse isOpen={tagListOpen}>
          <SelectableTagList />
        </Collapse>
      </CardBody>
    </Card>
  );
};

const SearchBar: FC = () => {
  const { setSearchString } = useContext(SearchContext);
  const onChange: ChangeEventHandler<HTMLInputElement> = (ev) => {
    setSearchString(ev.target.value);
  };
  return (
    <InputGroup>
      <Input
        placeholder="astrid.tech, CPE 233, React, etc."
        onChange={onChange}
      />
    </InputGroup>
  );
};

const SearchSection: FC = () => {
  return (
    <section className={styles.searchSection}>
      <Container>
        <Row>
          <Col xs={12} md={6} style={{ paddingBottom: 20 }}>
            <h2>Search</h2>
            <SearchBar />
            <CurrentlyUsedTagList />
          </Col>
          <Col xs={12} md={6}>
            <TagsFilterDropdown />
          </Col>
        </Row>
      </Container>
    </section>
  );
};

export const CardGroup: FC<{
  idPrefix: string;
  title?: { text: string; description?: string };
  projects: Project[];
}> = ({ title, idPrefix, projects }) => {
  var heading = null;
  if (title) {
    const infoId = `${idPrefix}-section-info`;
    heading = (
      <h3 className={styles.cardSectionTitle}>
        {title.text}{' '}
        {title.description ? (
          <>
            <FaInfoCircle
              title="Hover for information about this section"
              id={infoId}
            />
            <UncontrolledTooltip placement="right" target={infoId}>
              {title.description}
            </UncontrolledTooltip>
          </>
        ) : null}
      </h3>
    );
  }

  return (
    <section className={styles.cardGroupOuter}>
      {heading}
      <Row>
        {projects.map((project) => (
          <Col
            xs="12"
            md="6"
            xl="4"
            key={project.slug}
            className={styles.projectCardWrapper}
          >
            <ProjectCard project={project} />
          </Col>
        ))}
      </Row>
    </section>
  );
};

export const ProjectCardsView: FC = () => {
  const { isSearching, displayedProjects } = useContext(SearchContext);
  return (
    <Row>
      {displayedProjects.map((project) => (
        <Col
          xs="12"
          md="6"
          xl="4"
          key={project.slug}
          className={styles.projectCardWrapper}
        >
          <ProjectCard project={project} />
        </Col>
      ))}
    </Row>
  );
};

export const ProjectsIndex: FC<{
  projects: ProjectMeta[];
  fuseIndex: any;
  fuseKeys: string[];
}> = ({ projects, fuseIndex }) => {
  const index = Fuse.parseIndex<ProjectMeta>(fuseIndex);
  const fuse = new Fuse<ProjectMeta>(
    projects,
    {
      threshold: 0.4,
      keys: projects.map((p) => p.slug),
    },
    index
  );

  const title = 'Projects';
  const description =
    'An incomplete list of the projects I have worked on, of all sizes and types.';

  return (
    <Layout currentLocation="projects">
      <SEO title={title} description={description} />
      <PageHeading title={title} description={description} bgColor="#3baddd" />
      <main>
        <Filterer projects={projects} fuse={fuse}>
          <SearchSection />
          <div className={styles.projectsView}>
            <Container style={{ paddingTop: 10 }}>
              <ProjectCardsView />
            </Container>
          </div>
        </Filterer>
      </main>
    </Layout>
  );
};
