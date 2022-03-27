-- migrate:up

create table statics(
    static_id serial primary key,

    src_path text,
    content_hash text not null,
    uploaded_url text not null
);

create table images(
    id serial primary key,
    static_id integer references statics(static_id)
);

create table content(
    content_id serial primary key,

    file_type text not null,
    content text not null,
    src integer unique references statics(static_id),
    excerpt text
);

create table tags(
    tag_id serial primary key,

    slug text unique not null,
    title text,

    color text,
    bg_color text
);

create table tagged_objects(
    tagobj_id serial primary key
);

create table object_taggings(
    tagobj_id integer not null references tagged_objects(tagobj_id),
    tag_id integer not null references tags(tag_id),
    tag_ordinal integer not null,
    primary key (tagobj_id, tag_id, tag_ordinal),
    unique (tagobj_id, tag_ordinal)
);

create table blog_posts(
    id serial primary key,
    uuid uuid unique not null,

    title text,
    tagline text,

    thumbnail integer references images(id),

    slug_year integer not null,
    slug_month integer not null,
    slug_day integer not null,
    slug_ordinal integer not null,
    slug_title text,

    publish_time timestamp with time zone,
    create_time timestamp with time zone,
    modify_time timestamp with time zone,

    content_id integer not null unique references content(content_id),
    tagobj_id integer not null unique references tagged_objects(tagobj_id),

    unique (slug_year, slug_month, slug_day, slug_ordinal)
);

create view blog_posts_generated as
select
    id,
    format(
        '/%d/%02d/%02d/%d%s',
        slug_year,
        slug_month,
        slug_day,
        slug_ordinal,
        case
            when slug_title is null then ''
            else '/' || slug_title
        end
    ) page_url
from
    blog_posts;

create table projects(
    id serial primary key,
    uuid uuid unique not null,

    slug_title text unique not null,
    title text not null,
    tagline text not null,

    thumbnail integer references images(id),

    create_time timestamp with time zone not null,
    publish_time timestamp with time zone,
    modify_time timestamp with time zone,
    start_time timestamp with time zone,
    end_time timestamp with time zone,

    source_code text[] default '{}' not null,
    url text[] default '{}' not null,

    content_id integer unique references content(content_id),
    tagobj_id integer unique references tagged_objects(tagobj_id)
);

create view projects_generated as
select
    id,
    format('/projects/%s', slug_title) page_url
from
    projects;

create view tag_to_children as
with
    ptags as (
        select
            'project' otype,
            t.tag_id tag,
            t.slug tag_slug,
            p.title,
            p.tagline,
            p.thumbnail,
            p.end_time sort_date,
            g.page_url
        from projects p
            natural join projects_generated g
            natural join tags
            natural join object_taggings
            natural join tags t
    ),
    btags as (
        select 
            'blog' otype,
            t.tag_id tag,
            t.slug tag_slug,
            b.title,
            b.tagline,
            b.thumbnail,
            b.publish_time sort_date,
            g.page_url
        from blog_posts b
            natural join blog_posts_generated g
            natural join tagged_objects
            natural join object_taggings
            natural join tags t
    )
select *
from (
    select * from btags 
    union
    select * from ptags
) tagobjs
order by sort_date;

-- migrate:down

drop view tag_to_children;
drop view projects_generated;
drop table projects;
drop view blog_posts_generated ;
drop table blog_posts;
drop table object_taggings;
drop table tagged_objects;
drop table tags;
drop table content;
drop table images;
drop table statics;


