table! {
    image (name) {
        name -> Nullable<Text>,
    }
}

table! {
    post (uuid) {
        uuid -> Text,
        date -> Text,
        year -> Integer,
        month -> Integer,
        day -> Integer,
        ordinal -> Integer,
        title -> Nullable<Text>,
        description -> Nullable<Text>,
        short_name -> Nullable<Text>,
        published_date -> Nullable<Text>,
        updated_date -> Nullable<Text>,
        reply_to -> Nullable<Text>,
        bookmark_of -> Nullable<Text>,
        like_of -> Nullable<Text>,
        repost_of -> Nullable<Text>,
        rsvp -> Nullable<Text>,
        url -> Nullable<Text>,
        location -> Nullable<Text>,
        syndications -> Text,
        h_type -> Text,
        media -> Text,
        content -> Text,
        content_type -> Text,
    }
}

table! {
    project (uuid) {
        uuid -> Text,
        title -> Text,
        description -> Text,
        short_name -> Text,
        start_date -> Integer,
        end_date -> Nullable<Integer>,
        published_date -> Nullable<Integer>,
        updated_date -> Nullable<Integer>,
        url -> Nullable<Text>,
        status -> Nullable<Text>,
        source_urls -> Nullable<Text>,
        thumbnail -> Nullable<Text>,
        content -> Text,
        content_type -> Text,
    }
}

table! {
    tag (short_name) {
        short_name -> Text,
        name -> Nullable<Text>,
        background_color -> Nullable<Text>,
        color -> Nullable<Text>,
        description -> Text,
    }
}

table! {
    tag_to_object (object_uuid, tag) {
        object_uuid -> Text,
        tag -> Text,
    }
}

table! {
    tagged_object (uuid) {
        uuid -> Text,
        url -> Nullable<Text>,
    }
}

joinable!(post -> tagged_object (uuid));
joinable!(project -> image (thumbnail));
joinable!(project -> tagged_object (uuid));
joinable!(tag_to_object -> tag (tag));
joinable!(tag_to_object -> tagged_object (object_uuid));

allow_tables_to_appear_in_same_query!(
    image,
    post,
    project,
    tag,
    tag_to_object,
    tagged_object,
);
