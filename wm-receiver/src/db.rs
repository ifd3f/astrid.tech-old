use std::env;

use diesel::{sqlite::SqliteConnection, Connection};

pub fn get_db() -> SqliteConnection {
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    SqliteConnection::establish(&database_url)
        .expect(&format!("Error connecting to {}", database_url))
}

embed_migrations!();

pub fn run_migrations() {
    let db = get_db();
    embedded_migrations::run(&db).expect("Failure while running migrations");
}
