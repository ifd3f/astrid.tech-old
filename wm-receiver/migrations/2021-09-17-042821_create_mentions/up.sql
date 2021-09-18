CREATE TABLE mentions(
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    source_url TEXT NOT NULL,
    target_url TEXT NOT NULL,
    sender_ip TEXT NOT NULL,
    processing_status INTEGER NOT NULL,
    processing_attempts INTEGER NOT NULL DEFAULT 0,
    mentioned_on TIMESTAMP NOT NULL,
    processed_on TIMESTAMP,
    next_processing_attempt TIMESTAMP
);
