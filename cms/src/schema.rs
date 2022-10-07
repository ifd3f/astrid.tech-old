table! {
    content (id) {
        id -> Int4,
        content -> Text,
        content_type -> Text,
        rendered_html -> Nullable<Text>,
    }
}

table! {
    post (id) {
        id -> Int4,
        uuid -> Uuid,
        year -> Int4,
        month -> Int4,
        day -> Int4,
        ordinal -> Int4,
        title -> Nullable<Text>,
        description -> Nullable<Text>,
        slug -> Nullable<Text>,
        created_date -> Timestamp,
        published_date -> Nullable<Timestamp>,
        updated_date -> Nullable<Timestamp>,
        reply_to -> Nullable<Text>,
        bookmark_of -> Nullable<Text>,
        like_of -> Nullable<Text>,
        repost_of -> Nullable<Text>,
        rsvp -> Nullable<Text>,
        location -> Nullable<Text>,
        content -> Int4,
    }
}

table! {
    project (id) {
        id -> Int4,
        uuid -> Nullable<Uuid>,
        title -> Text,
        description -> Text,
        slug -> Text,
        start_date -> Int4,
        end_date -> Nullable<Int4>,
        published_date -> Nullable<Int4>,
        updated_date -> Nullable<Int4>,
        url -> Nullable<Text>,
        status -> Nullable<Text>,
        source_urls -> Nullable<Text>,
        thumbnail -> Nullable<Text>,
        content -> Int4,
    }
}

table! {
    tag (slug) {
        slug -> Text,
        name -> Nullable<Text>,
        background_color -> Nullable<Text>,
        color -> Nullable<Text>,
        description -> Text,
    }
}

table! {
    tag_to_object (object_uuid, tag) {
        object_uuid -> Uuid,
        tag -> Text,
    }
}

table! {
    tagged_object (uuid) {
        uuid -> Uuid,
        url -> Nullable<Text>,
    }
}

joinable!(post -> content (content));
joinable!(post -> tagged_object (uuid));
joinable!(project -> content (content));
joinable!(project -> tagged_object (uuid));
joinable!(tag_to_object -> tag (tag));
joinable!(tag_to_object -> tagged_object (object_uuid));

allow_tables_to_appear_in_same_query!(
    content,
    post,
    project,
    tag,
    tag_to_object,
    tagged_object,
);
