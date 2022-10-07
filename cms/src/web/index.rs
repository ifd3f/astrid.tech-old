use maud::html;
use navbar::NavbarItem;
use rocket::response::content;

use crate::web::components::navbar;

#[get("/")]
pub fn index() -> content::RawHtml<String> {
    let markup = html! {
        (navbar(Some(NavbarItem::Title)))
        h1 { "Welcome to my website" }
    };
    content::RawHtml(markup.into_string())
}
