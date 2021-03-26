// see http://tantek.pbworks.com/w/page/19402946/NewBase60
use rstest::rstest;

pub fn num_to_sxg(n: u128) -> String {
    static digits: &[u8; 60] = b"0123456789ABCDEFGHJKLMNPQRSTUVWXYZ_abcdefghijkmnopqrstuvwxyz";

    if n == 0 {
        return "0".to_string();
    }

    let mut n = n;
    let mut s = String::new();
    while n > 0 {
        let d = n % 60;
        let ch: char = digits[d as usize] as char;
        s.push(ch);
        n = (n - d) / 60;
    }
    s.chars().rev().collect()
}

pub fn sxg_to_num(s: &str) -> Option<u128> {
    let mut n: u128 = 0;
    for c in s.chars() {
        let digit = match c {
            '0'..='9' => c as u8 - b'0',
            'A'..='H' => c as u8 - b'A' + 10,
            'J'..='N' => c as u8 - b'J' + 18,
            'P'..='Z' => c as u8 - b'P' + 23,
            '_' => 34,
            'a'..='k' => c as u8 - b'a' + 35,
            'm'..='z' => c as u8 - b'm' + 46,
            'I' | 'l' => 1, // typo capital I, lowercase l to 1
            'O' => 0,       // error correct typo capital O to 0
            _ => continue,  // skip invalid chars
        };
        n = match n.checked_mul(60).and_then(|x| x.checked_add(digit as u128)) {
            Some(x) => x,
            None => return None,
        }
    }
    Some(n)
}

// Tests stolen from https://github.com/indieweb/newBase60py/blob/master/newbase60test.py

#[rstest(input, expected)]
#[case(0, "0")]
#[case(1, "1")]
#[case(60, "10")]
fn test_num_to_sxg(input: u128, expected: &str) {
    assert_eq!(num_to_sxg(input), expected)
}

#[rstest(input, expected)]
#[case("N", Some(22))]
#[case("H", Some(17))]
#[case("10", Some(60))]
#[case("NH", Some(1337))]
#[case("0", Some(0))]
#[case("asc", Some(129157))]
#[case("a̶̹͓̿̇̈́̔͗͝s̴̒̓̈͌̍̈́̀̐͂͛̑̊̿̑̈͜ͅc̷̢͙͔͈̠͎̱̭̭̏ͅ", Some(129157))]
#[case("1", Some(1))]
#[case("l", Some(1))]
#[case("l", Some(1))]
#[case("N🏳️‍⚧H", Some(1337))]
#[case("N̷̛͎̝͕͓͙̟̺͎̳̯̙͙̦̋͗͒̀̐̏̅͗̎̕̚͘ͅͅH̴̭̳͉͚͂̀̀̔", Some(1337))]
#[case("I", Some(1))]
#[case("O", Some(0))]
#[case("|", Some(0))]
#[case(",", Some(0))]
#[case("🥺", Some(0))]
#[case("sadfui9fasjf", Some(1908097676891172549880))]
#[case(
    "this is a very long string that will overflow the multiplication buffer",
    None
)]
fn test_sxg_to_num(input: &str, expected: Option<u128>) {
    assert_eq!(sxg_to_num(input), expected)
}

#[cfg(test)]
fn test_round_trip_n_s_n() {
    for n in 0..63832 as u128 {
        let s = num_to_sxg(n);
        assert_eq!(sxg_to_num(s.as_str()), Some(n))
    }
}
