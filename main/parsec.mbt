type ParseError String

struct ParseState {
  src : String
  ids : IDs
  loc : Loc
}

fn ParseState::new(src : String, ids : IDs) -> ParseState {
  ParseState::{ src : src, ids : ids, loc : Loc::new() }
}

fn cur(self : ParseState) -> Loc {
  self.loc.copy()
}

fn text(self : ParseState, start : Loc) -> String {
  let buf = Buffer::make(42)
  for i = start.pos; i < self.loc.pos; i = i + 1 {
    buf.write_char(self.src[i])
  }
  buf.to_string()
}

fn peek(self : ParseState) -> Option[Char] {
  if self.loc.pos >= self.src.length() {
    None
  } else {
    Some(self.src[self.loc.pos])
  }
}

fn next(self : ParseState) -> Bool {
  match self.peek() {
    None => false
    Some(ch) => 
      if ch == '\n' { 
        self.loc.next_line()
        true
      } else {
        self.loc.next_column()
        true
      }
  }
}

fn eat(self : ParseState, ch : Char) -> Result[ParseState, ParseError] {
  let loc = self.cur()
  match self.peek() {
    None => Err(ParseError("\(loc): expect \(ch) got None"))
    Some(n) => 
      if n == ch {
        self.next() |>  ignore
        Ok(self)
      } else {
        Err(ParseError("\(loc): expect \(ch) got \(n)"))
      }
  }
}

fn isSpace(ch : Char) -> Bool {
  ch == ' ' || ch == '\n' || ch == '\t'
}

fn skip_spaces(self : ParseState) -> ParseState {
  while true {
    let ch = self.peek()
    match ch {
      None => break
      Some(ch) => 
        if isSpace(ch) {
          self.next() |>  ignore
        } else {
          break
        }
    }
  }
  return self
}

struct Parser {
  label : String
  proc : (ParseState) -> Result[ParseState, ParseError]
}

