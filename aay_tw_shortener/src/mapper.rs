use crate::mapper::ShortCode::{Blog, Project};
use crate::mapper::ShortCodeParseError::{EmptyString, TooLong};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ShortCode<'a> {
    Blog(u32, u8, u8, Option<u8>),
    Project(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ShortCodeParseError {
    EmptyString,
    InvalidDate,
    InvalidChar,
    TooLong,
}

impl ShortCode<'_> {
    fn parse(shortcode: &'a str) -> Result<ShortCode<'a>, ShortCodeParseError> {
        if shortcode.len() > 12 {
            return Err(ShortCodeParseError::TooLong);
        }

        match shortcode.chars().next() {
            Some('b') => Ok(Blog(0, 0, 0, Some(0))),
            Some('p') => Ok(Project(&shortcode[1..])),
            _ => Err(ShortCodeParseError::EmptyString),
        }
    }

    fn expand(&self) -> String {
        match self {
            Blog(y, m, d, n) => {
                if let Some(n) = n {
                    format!("{}/{:0>2}/{:0>2}/{}", y, m, d, n)
                } else {
                    format!("{}/{:0>2}/{:0>2}", y, m, d)
                }
            }
            Project(p) => {
                format!("projects/{}", p)
            }
        }
    }
}

pub fn expand_shortcode(shortcode: &str) -> Result<String, ShortCodeParseError> {
    let parsed = ShortCode::parse(shortcode)?;
    Ok(parsed.expand())
}

#[test]
fn expands_blog_without_ord() {
    let code = Blog(2021, 3, 28, None);
    assert_eq!(code.expand(), "2021/03/28".to_string())
}

#[test]
fn expands_blog_with_ord() {
    let code = Blog(2021, 3, 28, Some(9));
    assert_eq!(code.expand(), "2021/03/28/9".to_string())
}

#[test]
fn expands_project() {
    let code = Project("asdf");
    assert_eq!(code.expand(), "projects/asdf".to_string())
}

#[test]
fn does_not_parse_empty_codes() {
    assert_eq!(ShortCode::parse(""), Err(EmptyString))
}

#[test]
fn does_not_parse_long_codes() {
    assert_eq!(ShortCode::parse("sfd8977f879978sdf}"), Err(TooLong))
}
