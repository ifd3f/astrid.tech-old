use crate::schema::mentions;

pub struct MentionConfig {
    pub allowed_target_hosts: HashSet<String>,
}

impl MentionConfig {
    pub fn create_mention(
        source_url: &'a str,
        target_url: &'a str,
        sender_ip: &'a str,
        processing_status: i32,
    ) -> Result<InsertMention<'a>, ()> {
        // TODO validate target URL is valid target

        Ok(InsertMention {
            source_url,
            target_url,
            sender_ip,
            processing_status,
        })
    }
}

#[derive(Insertable)]
#[table_name = "mentions"]
pub struct InsertMention<'a> {
    source_url: &'a str,
    target_url: &'a str,
    sender_ip: &'a str,
    processing_status: i32,
}
