table! {
    mentions (id) {
        id -> Nullable<Integer>,
        source_url -> Text,
        target_url -> Text,
        source_ip -> Text,
        processing_status -> Integer,
    }
}
