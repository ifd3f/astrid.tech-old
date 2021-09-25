SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: project_status; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.project_status AS ENUM (
    'early',
    'wip',
    'scrapped',
    'complete'
);


--
-- Name: rsvp; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.rsvp AS ENUM (
    'yes',
    'no',
    'maybe',
    'interested'
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: categories; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.categories (
    id integer NOT NULL,
    slug text NOT NULL,
    name text NOT NULL,
    backgroundcolor text NOT NULL,
    color text NOT NULL
);


--
-- Name: categories_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.categories_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: categories_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.categories_id_seq OWNED BY public.categories.id;


--
-- Name: entries; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.entries (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    year integer NOT NULL,
    month integer NOT NULL,
    day integer NOT NULL,
    ordinal integer NOT NULL,
    slug text,
    created_date timestamp with time zone NOT NULL,
    published_date timestamp with time zone NOT NULL,
    updated_date timestamp with time zone,
    name text,
    summary text,
    location text,
    photos text[] DEFAULT ARRAY[]::text[] NOT NULL,
    reply_to text[] DEFAULT ARRAY[]::text[] NOT NULL,
    repost_of text,
    rsvp public.rsvp,
    content text,
    colophon text,
    page_src_url text
);


--
-- Name: entries_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.entries_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: entries_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.entries_id_seq OWNED BY public.entries.id;


--
-- Name: entry_to_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.entry_to_category (
    entry_id integer NOT NULL,
    category_id integer NOT NULL,
    ordinal smallint NOT NULL
);


--
-- Name: project_to_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.project_to_category (
    project_id integer NOT NULL,
    category_id integer NOT NULL,
    ordinal smallint NOT NULL
);


--
-- Name: projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.projects (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    slug text NOT NULL,
    status public.project_status,
    featured_order smallint,
    started_date timestamp with time zone NOT NULL,
    finished_date timestamp with time zone,
    published_date timestamp with time zone NOT NULL,
    updated_date timestamp with time zone,
    name text NOT NULL,
    summary text,
    url text,
    source text,
    location text,
    content text NOT NULL
);


--
-- Name: projects_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.projects_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: projects_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.projects_id_seq OWNED BY public.projects.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: categories id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.categories ALTER COLUMN id SET DEFAULT nextval('public.categories_id_seq'::regclass);


--
-- Name: entries id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries ALTER COLUMN id SET DEFAULT nextval('public.entries_id_seq'::regclass);


--
-- Name: projects id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects ALTER COLUMN id SET DEFAULT nextval('public.projects_id_seq'::regclass);


--
-- Name: categories categories_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_pkey PRIMARY KEY (id);


--
-- Name: categories categories_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_slug_key UNIQUE (slug);


--
-- Name: entries entries_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_pkey PRIMARY KEY (id);


--
-- Name: entries entries_uuid_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_uuid_key UNIQUE (uuid);


--
-- Name: entry_to_category entry_category_ordering; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entry_to_category
    ADD CONSTRAINT entry_category_ordering UNIQUE (entry_id, ordinal);


--
-- Name: entry_to_category entry_to_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entry_to_category
    ADD CONSTRAINT entry_to_category_pkey PRIMARY KEY (category_id, entry_id);


--
-- Name: project_to_category project_category_ordering; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_to_category
    ADD CONSTRAINT project_category_ordering UNIQUE (project_id, ordinal);


--
-- Name: project_to_category project_to_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_to_category
    ADD CONSTRAINT project_to_category_pkey PRIMARY KEY (category_id, project_id);


--
-- Name: projects projects_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_pkey PRIMARY KEY (id);


--
-- Name: projects projects_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_slug_key UNIQUE (slug);


--
-- Name: projects projects_uuid_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_uuid_key UNIQUE (uuid);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: entry_to_category entry_to_category_category_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entry_to_category
    ADD CONSTRAINT entry_to_category_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.categories(id);


--
-- Name: entry_to_category entry_to_category_entry_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entry_to_category
    ADD CONSTRAINT entry_to_category_entry_id_fkey FOREIGN KEY (entry_id) REFERENCES public.entries(id);


--
-- Name: project_to_category project_to_category_category_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_to_category
    ADD CONSTRAINT project_to_category_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.categories(id);


--
-- Name: project_to_category project_to_category_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.project_to_category
    ADD CONSTRAINT project_to_category_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.projects(id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20210924224659');
