use maud::{html, Markup};

#[derive(Debug, PartialEq, Eq)]
pub enum NavbarItem {
    Title, Blog, Projects, Now, About
}

pub fn navbar(current_nav: Option<NavbarItem>) -> Markup {
    use NavbarItem::*;

    html! {
        nav class="navbar" {
            div class="left" {
                (navbar_btn("/", "astrid.tech", current_nav == Some(Title)))
                (navbar_btn("/blog", "Blog", current_nav == Some(Blog)))
                (navbar_btn("/projects", "Projects", current_nav == Some(Projects)))
            }
            div class="right" {
                (navbar_btn("/now", "Now", current_nav == Some(Now)))
                (navbar_btn("/about", "About", current_nav == Some(About)))
            }
        }
    }
}

fn navbar_btn(href: &str, title: &str, active: bool) -> Markup {
    html! {
        div class="navbtn" {
            a href=(href) { (title) }
        }
    }
}
