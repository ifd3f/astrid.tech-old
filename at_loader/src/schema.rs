table! {
    blog (id) {
        id -> Nullable<Integer>,
        title -> Text,
        slug -> Text,
        date -> Timestamp,
        content -> Text,
    }
}

table! {
    blog_to_tag (id) {
        id -> Nullable<Integer>,
        fk_blog -> Integer,
        fk_tag -> Text,
    }
}

table! {
    project (id) {
        id -> Nullable<Integer>,
        title -> Text,
        slug -> Text,
        start_date -> Timestamp,
        end_date -> Nullable<Timestamp>,
        url -> Nullable<Text>,
        status -> Nullable<Text>,
        source_url -> Nullable<Text>,
        thumbnail_path -> Nullable<Text>,
        content -> Text,
    }
}

table! {
    project_to_tag (id) {
        id -> Nullable<Integer>,
        fk_project -> Integer,
        fk_tag -> Text,
    }
}

table! {
    tag (code) {
        code -> Text,
        name -> Nullable<Text>,
        color -> Nullable<Integer>,
        background_color -> Nullable<Integer>,
    }
}

joinable!(blog_to_tag -> blog (fk_blog));
joinable!(project_to_tag -> project (fk_project));

allow_tables_to_appear_in_same_query!(
    blog,
    blog_to_tag,
    project,
    project_to_tag,
    tag,
);
