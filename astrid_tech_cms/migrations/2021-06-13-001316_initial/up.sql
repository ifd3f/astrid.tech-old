create table tagged_object
(
    uuid TEXT PRIMARY KEY NOT NULL,
    url  TEXT
);

create table tag
(
    short_name       TEXT PRIMARY KEY NOT NULL,
    name             TEXT,
    background_color TEXT,
    color            TEXT,
    description      TEXT             NOT NULL
);

create table tag_to_object
(
    id INTEGER PRIMARY KEY,
    object_uuid TEXT                NOT NULL,
    tag         TEXT                NOT NULL,

    foreign key (object_uuid) references tagged_object (uuid),
    foreign key (tag) references tag (short_name)
);

create table image
(
    name TEXT PRIMARY KEY
);

create table post
(
    uuid           TEXT PRIMARY KEY UNIQUE NOT NULL,

    date           TEXT                    NOT NULL,
    year           INTEGER                 NOT NULL,
    month          INTEGER                 NOT NULL,
    day            INTEGER                 NOT NULL,
    ordinal        INTEGER                 NOT NULL,

    title          TEXT,
    description    TEXT,
    short_name     TEXT,

    published_date TEXT,
    updated_date   TEXT,

    reply_to       TEXT,
    bookmark_of    TEXT,
    like_of        TEXT,
    repost_of      TEXT,
    rsvp           TEXT,
    url            TEXT,
    location       TEXT,

    syndications   TEXT                    NOT NULL, -- a json array
    h_type         TEXT                    NOT NULL,
    media          TEXT                    NOT NULL,

    content        TEXT                    NOT NULL,
    content_type   TEXT                    NOT NULL,

    foreign key (uuid) references tagged_object (uuid),
    unique (year, month, day, ordinal)
);

CREATE TABLE project
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
    content_type   TEXT                    NOT NULL,

    foreign key (uuid) references tagged_object (uuid),
    foreign key (thumbnail) references image (name)
);

CREATE UNIQUE INDEX post_ymdo_idx ON post (year, month, day, ordinal, short_name);
