pub struct DatedSlug {
    pub year: i32,
    pub month: u32,
    pub day: u32,
    pub ordinal: u32,
    pub short_name: String,
}

pub trait TimestreamObject {
    /// Build a dated slug from this object. Returns a slug if the provided
    /// year, month, day, and ordinal are valid, otherwise returns `Nothing`.
    fn get_slug(&self, year: i32, month: u32, day: u32, ordinal: u32) -> Option<DatedSlug>;

    fn get_preview(&self);

    fn get_page(&self);
}