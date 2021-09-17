use crate::schema::mentions;

#[derive(Insertable)]
#[table_name = "mentions"]
pub struct InsertMention<'a> {
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32
}
