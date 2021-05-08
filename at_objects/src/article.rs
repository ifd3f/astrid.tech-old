use std::path::PathBuf;

use chrono::Datelike;

use at_loader::input_types::ArticleMeta;
use crate::timestream::{DatedSlug, TimestreamObject};
use at_loader::page::Page;

pub struct Article {
    meta: ArticleMeta,
    raw_content: String,
    asset_dir: PathBuf,
    page: Page,
}

impl TimestreamObject for Article {
    fn get_slug(&self, year: i32, month: u32, day: u32, ordinal: u32) -> Option<DatedSlug> {
        let date = &self.meta.date.date();

        if (date.year(), date.month(), date.day()) == (year, month, day) {
            Some(DatedSlug {
                year,
                month,
                day,
                ordinal,
                short_name: self.page.short_name.to_string(),
            })
        } else {
            None
        }
    }

    fn get_preview(&self) {
        unimplemented!()
    }

    fn get_page(&self) {
        unimplemented!()
    }
}