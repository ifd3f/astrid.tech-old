table! {
    mentions (id) {
        id -> Nullable<Integer>,
        source_url -> Text,
        target_url -> Text,
        sender_ip -> Text,
        processing_status -> Integer,
    }
}
