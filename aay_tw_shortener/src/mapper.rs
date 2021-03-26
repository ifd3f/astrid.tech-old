enum ShortCode<'a> {
    Blog(u32),
    Project(&'a str),
}

fn parse_shortcode(shortcode: &'a str) -> ShortCode<'a> {
    match shortcode[0] {
        'b' => Blog(0),
        'p' => Project(shortcode[1..]),
    }
}

fn translate_shortcode(shortcode: &str) -> String {}
