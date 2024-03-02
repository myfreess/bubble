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
  for i = start.pos, i < self.loc.pos, i = i + 1 {
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

fn next(self : ParseState) -> Option[Char] {
  match self.peek() {
    None => None
    Some(ch) => 
      if ch == '\n' { 
        self.loc.next_line()
      } else {
        self.loc.next_column()
      }
  }
}

struct Parser {
  label : String
  proc : (ParseState) -> Result[ParseState, ParseError]
}

