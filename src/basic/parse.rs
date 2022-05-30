use std::iter::from_fn;

#[derive(Debug)]
pub enum Token<'a> {
    Name(&'a str),
    LitI32(i32),
    LitBool(bool),
    LitStr(&'a str),
    Special(&'a str),
}

fn next_token<'a>(s: &mut &'a str) -> Option<Token<'a>> {
    *s = s.trim_start();
    if s.is_empty() {
        return None;
    }
    match s.split_whitespace().next().unwrap() {
        special @ ("prod" | "sum" | "func" | "tag" | "do" | "end" | "if" | "else" | "while"
        | "break" | "continue" | "return" | "let" | "is") => {
            *s = s.strip_prefix(special).unwrap();
            Some(Token::Special(special))
        }
        "true" => {
            *s = s.strip_prefix("true").unwrap();
            Some(Token::LitBool(true))
        }
        "false" => {
            *s = s.strip_prefix("false").unwrap();
            Some(Token::LitBool(false))
        }
        lit_i32 if lit_i32.starts_with(char::is_numeric) => {
            let lit_i32 = s.split(|c: char| !c.is_numeric()).next().unwrap();
            let token = Token::LitI32(lit_i32.parse().unwrap());
            *s = s.strip_prefix(lit_i32).unwrap();
            Some(token)
        }
        lit_str if lit_str.starts_with('"') => {
            let mut escaping = false;
            let (lit_str, next_s) = s[1..]
                .split_once(|c| {
                    let res = c == '"' && !escaping;
                    escaping = if c == '\\' { !escaping } else { false };
                    res
                })
                .unwrap();
            *s = next_s;
            Some(Token::LitStr(lit_str))
        }
        special if ["==", "!="].into_iter().any(|s| special.starts_with(s)) => {
            *s = s.strip_prefix(special).unwrap();
            Some(Token::Special(special))
        }
        special if special.starts_with(['=', '+', '-', '*', '/', '%', ',', '(', ')', '<', '>']) => {
            let (special, next_s) = s.split_at(1);
            *s = next_s;
            Some(Token::Special(special))
        }
        name => {
            let name = name
                .split(|c: char| !c.is_alphanumeric() && c != '.' && c != '_' && c != ':')
                .next()
                .unwrap();
            assert!(!name.is_empty());
            *s = s.strip_prefix(name).unwrap();
            Some(Token::Name(name))
        }
    }
}

pub fn iter_token(mut s: &str) -> impl Iterator<Item = Token<'_>> {
    from_fn(move || next_token(&mut s))
}
