-- migrate:up
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE rsvp AS ENUM ('yes', 'no', 'maybe', 'interested');

create table entries(
    -- Ephemeral primary key. DB-internal ONLY. Do not expose.
    id serial primary key not null,

    -- UUID that uniquely identifies this post across space and time.
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
    photo text[] not null default array[]::text[],

    -- Misc. properties
    reply_to text[] not null default array[]::text[],
    repost_of text,
    rsvp rsvp,

    -- Content, as HTML.
    content text
);

-- migrate:down
drop table entries;
drop type rsvp;
