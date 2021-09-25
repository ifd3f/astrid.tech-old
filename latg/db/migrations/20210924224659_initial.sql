-- migrate:up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE rsvp AS ENUM ('yes', 'no', 'maybe', 'interested');

create table entries(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- UUID that uniquely identifies this entry across space and time.
    uuid uuid unique not null,

    -- User-friendly slug fields.
    year integer not null,
    month integer not null,
    day integer not null,
    ordinal integer not null,
    slug text,

    -- Actual date fields.
    created_date timestamp with time zone not null,
    published_date timestamp with time zone not null,
    updated_date timestamp with time zone,

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
    rsvp rsvp,

    -- Content, as HTML.
    content text,

    -- Info about the app that was used to generate this entry.
    colophon text,

    -- Download link for the source of this page
    page_src_url text
);

CREATE TYPE project_status AS ENUM ('early', 'wip', 'scrapped', 'complete');

create table projects(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- UUID that uniquely identifies this project across space and time.
    uuid uuid unique not null,

    -- Slug of this project.
    slug text unique not null,

    status project_status,

    -- If null, do not feature. If not null, picks the order.
    featured_order smallint,

    started_date timestamp with time zone not null,
    finished_date timestamp with time zone,
    sort_date timestamp with time zone not null,
    published_date timestamp with time zone not null,
    updated_date timestamp with time zone,

    -- The name of this project.
    name text not null,
    
    -- A tagline for this project.
    summary text,

    -- A URL to the project on the internet.
    url text,

    -- A URL to the project's source code.
    source text,

    -- A URI describing the physical location.
    location text,

    -- Content, as HTML.
    content text not null
);

create table categories(
    -- Primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    slug text unique not null,
    name text not null,
    backgroundColor text not null,
    color text not null
);

create table entry_to_category(
    entry_id integer references entries (id),
    category_id integer references categories (id),

    -- Used for ordering the categories.
    ordinal smallint not null,

    constraint entry_to_category_pkey primary key (category_id, entry_id),
    constraint entry_category_ordering unique (entry_id, ordinal)
);

create table project_to_category(
    project_id integer references projects (id),
    category_id integer references categories (id),

    -- Used for ordering the categories.
    ordinal smallint not null,

    constraint project_to_category_pkey primary key (category_id, project_id),
    constraint project_category_ordering unique (project_id, ordinal)
);

-- migrate:down
drop table project_to_category;
drop table entry_to_category;
drop table categories;
drop table projects;
drop table entries;

drop type rsvp;
drop type project_status;
drop extension "uuid-ossp";
