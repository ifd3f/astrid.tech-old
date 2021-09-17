CREATE TABLE mentions(
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_url TEXT NOT NULL,
    target_url TEXT NOT NULL,
    sender_ip TEXT NOT NULL,
    processing_status INTEGER NOT NULL
);
