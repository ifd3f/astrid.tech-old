table! {
    Images (name) {
        name -> Nullable<Text>,
    }
}

table! {
    Posts (uuid) {
        uuid -> Text,
        year -> Integer,
        month -> Integer,
        day -> Integer,
        ordinal -> Integer,
        title -> Nullable<Text>,
        description -> Nullable<Text>,
        short_name -> Nullable<Text>,
        date -> Integer,
        published_date -> Nullable<Integer>,
        updated_date -> Nullable<Integer>,
        reply_to -> Nullable<Text>,
        bookmark_of -> Nullable<Text>,
        like_of -> Nullable<Text>,
        repost_of -> Nullable<Text>,
        rsvp -> Nullable<Text>,
        url -> Nullable<Text>,
        location -> Nullable<Text>,
        tags -> Text,
        syndications -> Text,
        h_type -> Text,
        media -> Nullable<Text>,
        content_type -> Text,
        content -> Text,
    }
}

table! {
    Projects (uuid) {
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
    }
}

table! {
    TagToObject (id) {
        id -> Integer,
        object_uuid -> Text,
        tag -> Text,
    }
}

table! {
    TaggedObjects (uuid) {
        uuid -> Text,
        url -> Text,
        #[sql_name = "type"]
        type_ -> Text,
    }
}

table! {
    Tags (short_name) {
        short_name -> Text,
        name -> Nullable<Text>,
        background_color -> Nullable<Text>,
        color -> Nullable<Text>,
        description -> Nullable<Text>,
    }
}

joinable!(Posts -> TaggedObjects (uuid));
joinable!(Projects -> Images (thumbnail));
joinable!(Projects -> TaggedObjects (uuid));
joinable!(TagToObject -> TaggedObjects (object_uuid));
joinable!(TagToObject -> Tags (tag));

allow_tables_to_appear_in_same_query!(
    Images,
    Posts,
    Projects,
    TagToObject,
    TaggedObjects,
    Tags,
);
