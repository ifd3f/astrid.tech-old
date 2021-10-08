-- migrate:up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE rsvp_value AS ENUM ('yes', 'no', 'maybe', 'interested');
CREATE TYPE doc_type AS ENUM ('h-entry', 'x-project');

create table documents(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- UUID that uniquely identifies this document across space and time.
    uuid uuid unique not null,

    -- Actual date fields.
    created_date timestamp with time zone not null,
    published_date timestamp with time zone not null,
    updated_date timestamp with time zone,

    -- Canonical URL to the content.
    canonical_url text not null,

    -- Type of this document.
    doc_type doc_type not null,

    -- Content, as HTML.
    content text,

    -- Info about the app that was used to generate this entry.
    colophon text,

    -- Download link for the source of this page, or null for non-downloadable
    page_src_url text
);

create table entries(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- The document this entry is attached to.
    document integer unique references documents (id),

    -- User-friendly slug fields.
    year integer not null,
    month integer not null,
    day integer not null,
    ordinal integer not null,
    short_name text,

    -- The name of this entry.
    name text,
    
    -- A tagline for this entry.
    summary text,

    -- A URI describing the location.
    location text,

    -- Photos
    photos text[] not null default array[]::text[],

    -- Misc. properties
    reply_to text[] not null default array[]::text[],
    repost_of text,

    rsvp rsvp_value,

    constraint entry_slug unique (year, month, day, ordinal)
);

CREATE TYPE project_status AS ENUM ('early', 'wip', 'scrapped', 'complete');

create table projects(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- The document this project is attached to.
    document integer unique references documents (id),

    -- Slug of this project.
    slug text unique not null,

    -- The status of this project
    status project_status,

    -- If null, do not feature. If not null, picks the order.
    featured_order smallint,

    started_date timestamp with time zone not null,
    finished_date timestamp with time zone,
    sort_date timestamp with time zone not null,

    -- The name of this project.
    name text not null,
    
    -- A tagline for this project.
    summary text,

    -- A URL to the project on the internet.
    url text,

    -- A URL to the project's source code.
    source text,

    -- A URI describing the physical location.
    location text
);

create table categories(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    slug text unique not null,
    name text not null,
    backgroundColor text not null,
    color text not null
);

create table document_to_category(
    document_id integer references documents (id),
    category_id integer references categories (id),

    -- Used for ordering the categories.
    ordinal smallint not null,

    constraint document_to_category_pkey primary key (category_id, document_id),
    constraint document_category_ordering unique (document_id, ordinal)
);

-- migrate:down
drop table document_to_category;
drop table categories;
drop table projects;
drop table entries;
drop table documents;

drop type rsvp_value;
drop type project_status;
drop type doc_type;
drop extension "uuid-ossp";
