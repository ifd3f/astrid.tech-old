table! {
    requests (id) {
        id -> Integer,
        source_url -> Text,
        target_url -> Text,
        sender_ip -> Text,
        processing_status -> Integer,
        processing_attempts -> Integer,
        mentioned_on -> Timestamp,
        processed_on -> Nullable<Timestamp>,
        next_processing_attempt -> Nullable<Timestamp>,
    }
}
