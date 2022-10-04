use std::str::FromStr;

use chrono::{Datelike, DateTime, FixedOffset, Utc};
use diesel::{Insertable, Queryable, RunQueryDsl, Connection};
use diesel::SqliteConnection;
use serde::Serialize;
use serde_json::ser::CharEscape::Tab;
use url::Url;
use uuid::Uuid;

use crate::content;
use crate::content::content::ContentType;
use crate::content::post::Post;
use crate::schema::{post, tag, tagged_object, tag_to_object};
use crate::schema::post::columns::h_type;
use diesel::associations::HasTable;
use std::borrow::Borrow;
use std::sync::Arc;


#[derive(Insertable, Debug)]
#[table_name = "post"]
struct InsertPost {
    uuid: String,

    date: String,
    year: i32,
    month: i32,
    day: i32,
    ordinal: i32,

    title: Option<String>,
    description: Option<String>,
    short_name: Option<String>,

    published_date: Option<String>,
    updated_date: Option<String>,

    reply_to: Option<String>,
    bookmark_of: Option<String>,
    like_of: Option<String>,
    repost_of: Option<String>,
    rsvp: Option<String>,
    url: Option<String>,
    location: Option<String>,

    syndications: String,
    h_type: String,
    media: String,

    content: String,
    content_type: String,
}

#[derive(Insertable, Queryable, Debug)]
#[table_name = "tag"]
struct Tag {
    short_name: String,
    name: Option<String>,
    background_color: Option<String>,
    color: Option<String>,
    description: String,
}

impl Tag {
    pub fn from_shortname(short_name: String) -> Tag {
        Tag {
            short_name,
            name: None,
            background_color: None,
            color: None,
            description: "".to_string(),
        }
    }
}

#[derive(Insertable, Debug)]
#[table_name = "tagged_object"]
struct TaggedObject {
    uuid: String,
    url: Option<String>,
}

#[derive(Insertable, Debug)]
#[table_name = "tag_to_object"]
struct TagToObject {
    object_uuid: String,
    tag: String,
}

impl From<Post> for InsertPost {
    fn from(post: Post) -> Self {
        let slug = post.get_slug();
        let Post { meta, content } = post;
        let date = meta.date;
        InsertPost {
            uuid: meta.uuid.to_string(),
            date: date.to_rfc3339(),
            year: slug.year,
            month: slug.month as i32,
            day: slug.day as i32,
            ordinal: slug.ordinal as i32,
            title: meta.title,
            description: meta.description,
            short_name: meta.short_name,
            published_date: meta.published_date.map(|x| x.to_rfc3339()),
            updated_date: meta.updated_date.map(|x| x.to_rfc3339()),
            reply_to: meta.reply_to.map(|x| x.to_string()),
            bookmark_of: None,
            like_of: None,
            repost_of: None,
            rsvp: None,
            url: None,
            location: None,
            syndications: serde_json::to_string(&meta.syndications).unwrap(),
            h_type: serde_json::to_string(&meta.h_type).unwrap(),
            media: serde_json::to_string(&meta.media).unwrap(),
            content: content.content,
            content_type: content.content_type.to_mimetype().to_string(),
        }
    }
}

impl Into<Post> for InsertPost {
    fn into(self) -> Post {
        let meta = content::post::PostMeta {
            h_type: serde_json::from_str(self.h_type.as_str()).unwrap(),
            title: self.title,
            description: self.description,
            short_name: self.short_name,
            uuid: Uuid::from_str(self.uuid.as_str()).unwrap(),
            date: DateTime::parse_from_rfc3339(self.date.as_str()).unwrap(),
            published_date: None,
            updated_date: None,
            ordinal: self.ordinal as u32,
            reply_to: None,
            repost_of: None,
            tags: vec![],
            syndications: serde_json::from_str(&*self.syndications).unwrap(),
            media: serde_json::from_str(&*self.media).unwrap(),
        };
        let content = content::content::Content {
            content_type: ContentType::from_mimetype(self.content_type.as_str()).unwrap(),
            content: self.content,
        };
        Post { meta, content }
    }
}

pub fn store_post(conn: &SqliteConnection, post: Post) {
    let uuid_str = post.meta.uuid.to_string();
    let to = TaggedObject { uuid: uuid_str.clone(), url: None };
    let tags: Vec<Tag> = post.meta.tags.iter()
        .map(|t| Tag::from_shortname(t.clone()))
        .collect();
    let links: Vec<TagToObject> = post.meta.tags.iter()
        .map(|t| TagToObject { object_uuid: uuid_str.clone(), tag: t.clone() })
        .collect();

    conn.transaction::<(), diesel::result::Error, _>(|| {
        diesel::insert_into(tagged_object::table)
            .values(&to)
            .execute(conn)?;

        diesel::insert_or_ignore_into(tag::table)
            .values(&tags)
            .execute(conn)?;

        diesel::insert_into(post::table)
            .values(&InsertPost::from(post))
            .execute(conn)?;

        diesel::insert_into(tag_to_object::table)
            .values(&links)
            .execute(conn)?;

        Ok(())
    }).unwrap();
}


#[cfg(test)]
mod tests {
    use diesel::{ Identifiable, Queryable, Connection, QueryDsl, RunQueryDsl, SqliteConnection};
    use diesel::sqlite::Sqlite;
    use diesel::prelude::*;

    use crate::content::index::{InsertPost, store_post};
    use crate::content::post::test::get_working_combined_post;
    use crate::schema;
    use crate::schema::tagged_object;
    use std::sync::Arc;

    #[derive(Identifiable, Queryable)]
    struct QueryPost {
        uuid: String,
        year: i32,
        month: i32,
        day: i32
    }

    #[derive(Identifiable, Queryable)]
    struct QueryTag {
        id: String
    }

    #[derive(Associations, Identifiable, Queryable)]
    #[belongs_to(QueryPost)]
    #[belongs_to(QueryTag)]
    #[table_name = "tagged_object"]
    struct QueryTaggedObject {
        id: i32,
        query_post_id: String,
        url: String
    }

    #[test]
    fn stores_posts() {
        let conn = SqliteConnection::establish("index.sqlite3").unwrap();
        let obj = get_working_combined_post();
        let uuid_str = obj.meta.uuid.to_string();

         conn.test_transaction(|| {
             store_post(&conn, obj);

             find(uuid_str.as_str())
                 .get_result::<QueryPost>(&conn);

             Ok(()) as Result<(), ()>
         });
    }
}