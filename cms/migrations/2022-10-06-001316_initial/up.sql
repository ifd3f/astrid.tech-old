create table tagged_object
(
    uuid UUID PRIMARY KEY NOT NULL,
    url  TEXT
);

create table tag
(
    slug             TEXT PRIMARY KEY NOT NULL,
    name             TEXT,
    background_color TEXT,
    color            TEXT,
    description      TEXT             NOT NULL
);

create table tag_to_object
(
    object_uuid UUID                NOT NULL references tagged_object (uuid),
    tag         TEXT                NOT NULL references tag(slug),
    PRIMARY KEY (object_uuid, tag)
);

CREATE table content
(
    id INTEGER PRIMARY KEY,

    content        TEXT                    NOT NULL,
    content_type   TEXT                    NOT NULL,

    rendered_html  TEXT                    NULL
);

create table post
(
    id             INTEGER             PRIMARY KEY,
    uuid           UUID                UNIQUE NOT NULL references tagged_object(uuid),

    year           INTEGER                 NOT NULL GENERATED ALWAYS AS ( DATE_PART('year', created_date)) STORED,
    month          INTEGER                 NOT NULL GENERATED ALWAYS AS ( DATE_PART('month', created_date)) STORED,
    day            INTEGER                 NOT NULL GENERATED ALWAYS AS ( DATE_PART('day', created_date)) STORED,
    ordinal        INTEGER                 NOT NULL,

    title          TEXT,
    description    TEXT,
    slug           TEXT,

    created_date   timestamp not null,
    published_date timestamp,
    updated_date   timestamp,

    reply_to       TEXT,
    bookmark_of    TEXT,
    like_of        TEXT,
    repost_of      TEXT,
    rsvp           TEXT,
    location       TEXT,

    content        INTEGER NOT NULL references content (id),

    UNIQUE (year, month, day, ordinal)
);

CREATE TABLE project
(
    id             INTEGER                 PRIMARY KEY,
    uuid           UUID UNIQUE references tagged_object (uuid),

    title          TEXT                    NOT NULL,
    description    TEXT                    NOT NULL,
    slug           TEXT UNIQUE             NOT NULL,

    start_date     INTEGER                 NOT NULL,
    end_date       INTEGER,
    published_date INTEGER,
    updated_date   INTEGER,

    url            TEXT,
    status         TEXT,
    source_urls    TEXT,
    thumbnail      TEXT,

    content        INTEGER NOT NULL references content (id)
);
