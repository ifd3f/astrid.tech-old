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
-- Name: doc_type; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.doc_type AS ENUM (
    'h-entry',
    'x-project'
);


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
-- Name: rsvp_value; Type: TYPE; Schema: public; Owner: -
--

CREATE TYPE public.rsvp_value AS ENUM (
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
-- Name: document_to_category; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.document_to_category (
    document_id integer NOT NULL,
    category_id integer NOT NULL,
    ordinal smallint NOT NULL
);


--
-- Name: documents; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.documents (
    id integer NOT NULL,
    uuid uuid NOT NULL,
    created_date timestamp with time zone NOT NULL,
    published_date timestamp with time zone NOT NULL,
    updated_date timestamp with time zone,
    canonical_url text NOT NULL,
    doc_type public.doc_type NOT NULL,
    content text,
    colophon text,
    page_src_url text
);


--
-- Name: documents_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.documents_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: documents_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.documents_id_seq OWNED BY public.documents.id;


--
-- Name: entries; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.entries (
    id integer NOT NULL,
    document integer,
    year integer NOT NULL,
    month integer NOT NULL,
    day integer NOT NULL,
    ordinal integer NOT NULL,
    slug text,
    name text,
    summary text,
    location text,
    photos text[] DEFAULT ARRAY[]::text[] NOT NULL,
    reply_to text[] DEFAULT ARRAY[]::text[] NOT NULL,
    repost_of text,
    rsvp public.rsvp_value,
    rsvp_to text
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
-- Name: projects; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.projects (
    id integer NOT NULL,
    document integer,
    slug text NOT NULL,
    status public.project_status,
    featured_order smallint,
    started_date timestamp with time zone NOT NULL,
    finished_date timestamp with time zone,
    sort_date timestamp with time zone NOT NULL,
    name text NOT NULL,
    summary text,
    url text,
    source text,
    location text
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
-- Name: documents id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.documents ALTER COLUMN id SET DEFAULT nextval('public.documents_id_seq'::regclass);


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
-- Name: document_to_category document_category_ordering; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document_to_category
    ADD CONSTRAINT document_category_ordering UNIQUE (document_id, ordinal);


--
-- Name: document_to_category document_to_category_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document_to_category
    ADD CONSTRAINT document_to_category_pkey PRIMARY KEY (category_id, document_id);


--
-- Name: documents documents_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.documents
    ADD CONSTRAINT documents_pkey PRIMARY KEY (id);


--
-- Name: documents documents_uuid_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.documents
    ADD CONSTRAINT documents_uuid_key UNIQUE (uuid);


--
-- Name: entries entries_document_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_document_key UNIQUE (document);


--
-- Name: entries entries_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_pkey PRIMARY KEY (id);


--
-- Name: entries entry_slug; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entry_slug UNIQUE (year, month, day, ordinal);


--
-- Name: projects projects_document_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_document_key UNIQUE (document);


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
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: document_to_category document_to_category_category_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document_to_category
    ADD CONSTRAINT document_to_category_category_id_fkey FOREIGN KEY (category_id) REFERENCES public.categories(id);


--
-- Name: document_to_category document_to_category_document_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.document_to_category
    ADD CONSTRAINT document_to_category_document_id_fkey FOREIGN KEY (document_id) REFERENCES public.documents(id);


--
-- Name: entries entries_document_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_document_fkey FOREIGN KEY (document) REFERENCES public.documents(id);


--
-- Name: projects projects_document_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.projects
    ADD CONSTRAINT projects_document_fkey FOREIGN KEY (document) REFERENCES public.documents(id);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20210924224659');
