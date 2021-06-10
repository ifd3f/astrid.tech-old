use rocket_contrib::templates::Template;

pub struct DateSlug {
    year: u32,
    month: u32,
    day: u32,
    ord: u32,
    name: String
}

impl DateSlug {
    pub fn new(year: u32, month: u32, day: u32, ord: u32, name: String) -> DateSlug {
        DateSlug { year, month, day, ord, name }
    }
}

pub enum PartialSlug<'a> {
    Y(u32),
    M(u32, u32),
    D(u32, u32, u32),
    O(u32, u32, u32, u32),
    F(u32, u32, u32, u32, &'a str)
}

pub trait PostRegistry<'a, T: Post> {
    fn find(slug: &PartialSlug) -> dyn Iterator<Item=DateSlug>;
    fn get(slug: &DateSlug) -> &'a T;
}

pub trait Post {
    fn render_body() -> Template;
}

pub enum PostUnion<A, B> {
    PA(A),
    PB(B)
}

pub enum PostRegistryUnion<A, B> {
    PA(A),
    PB(B)
}

impl PostRegistry<PostUnion<A, B>> for PostUnion<A, B> {
    fn find(slug: &PartialSlug<'a>) -> &dyn Iterator<Item=DateSlug> {
        slug.
    }

    fn get(slug: &DateSlug) -> &PostUnion<_, _> {
        unimplemented!()
    }
}