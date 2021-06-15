create table TaggedObjects
(
    uuid TEXT PRIMARY KEY NOT NULL,
    url  TEXT             NOT NULL,
    type TEXT             NOT NULL
);

create table Tags
(
    short_name       TEXT PRIMARY KEY NOT NULL,
    name             TEXT,
    background_color TEXT,
    color            TEXT,
    description      TEXT
);

create table TagToObject
(
    id          INTEGER PRIMARY KEY NOT NULL,
    object_uuid TEXT                NOT NULL,
    tag         TEXT                NOT NULL,

    foreign key (object_uuid) references TaggedObjects (uuid),
    foreign key (tag) references Tags (short_name)
);

create table Images
(
    name TEXT PRIMARY KEY
);

create table Content
(
    path TEXT PRIMARY KEY UNIQUE NOT NULL,
    content                      NOT NULL,
    content_type                 NOT NULL
);

create table Posts
(
    uuid           TEXT PRIMARY KEY UNIQUE NOT NULL,
    year           INTEGER                 NOT NULL,
    month          INTEGER                 NOT NULL,
    day            INTEGER                 NOT NULL,
    ordinal        INTEGER                 NOT NULL,

    title          TEXT,
    description    TEXT,
    short_name     TEXT,

    date           INTEGER                 NOT NULL,
    published_date INTEGER,
    updated_date   INTEGER,

    reply_to       TEXT,
    bookmark_of    TEXT,
    like_of        TEXT,
    repost_of      TEXT,
    rsvp           TEXT,
    url            TEXT,
    location       TEXT,

    tags           TEXT                    NOT NULL,
    syndications   TEXT                    NOT NULL, -- a json array
    h_type         TEXT                    NOT NULL,
    media          TEXT,

    content        TEXT                    NOT NULL,

    foreign key (uuid) references TaggedObjects (uuid),
    foreign key (content) references Content (path),
    unique (year, month, day, ordinal)
);

CREATE TABLE Projects
(
    uuid           TEXT PRIMARY KEY UNIQUE NOT NULL,

    title          TEXT                    NOT NULL,
    description    TEXT                    NOT NULL,
    short_name     TEXT UNIQUE             NOT NULL,

    start_date     INTEGER                 NOT NULL,
    end_date       INTEGER,
    published_date INTEGER,
    updated_date   INTEGER,

    url            TEXT,
    status         TEXT,
    source_urls    TEXT,
    thumbnail      TEXT,
    content        TEXT                    NOT NULL,

    foreign key (content) references Content (path),
    foreign key (uuid) references TaggedObjects (uuid),
    foreign key (thumbnail) references Images (name)
);
