enum ErrorKind {
  Lethal
  Recoverable
}

type ParseError (ErrorKind, String)

struct ParseState {
  src : String
  ids : IDs
  loc : Loc
  mut last_err : Option[ParseError]
}

fn ParseState::new(src : String, ids : IDs) -> ParseState {
  ParseState::{ src : src, ids : ids, loc : Loc::new(), last_err : None }
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

fn eat(self : ParseState, ch : Char) -> Result[Unit, ParseError] {
  let loc = self.cur()
  match self.peek() {
    None => Err(ParseError((Recoverable, "\(loc): expect \(ch) got None")))
    Some(n) => 
      if n == ch {
        self.next() |>  ignore
        Ok(())
      } else {
        Err(ParseError((Recoverable, "\(loc): expect \(ch) got \(n)")))
      }
  }
}

fn isSpace(ch : Char) -> Bool {
  ch == ' ' || ch == '\n' || ch == '\t'
}

fn skip_spaces(self : ParseState) {
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
}

fn back(self : ParseState, loc : Loc, e : ParseError) {
  // 恢复到原位置
  // 状态转移：修改loc和last_err
  self.loc.col = loc.col
  self.loc.ln  = loc.ln
  self.loc.pos = loc.pos
  // 记录错误信息
  self.last_err = Some(e)
}

fn soi(self : ParseState) -> Result[Unit, ParseError] {
  // 期盼当前解析状态为起始状态
  // 状态转移：无
  if self.loc.pos != 0 {
    let loc = self.loc
    Err(ParseError((Lethal, "\(loc) : expected start of input")))
  } else {
    Ok(())
  }
}

fn eoi(self : ParseState) -> Result[Unit, ParseError] {
  // 期盼当前解析状态为结束状态
  // 状态转移：无
  if self.loc.pos != self.src.length() {
    let loc = self.loc
    match self.last_err {
      None => Err(ParseError((Lethal, "\(loc) : expected end of input")))
      Some(ParseError((_, msg))) => Err(ParseError((Lethal, msg)))
    }
  } else {
    Ok(())
  }
}

struct Parser {
  label : String
  proc : (ParseState) -> Result[Unit, ParseError]
}

fn Parser::new(label : String, proc : (ParseState) -> Result[Unit, ParseError]) -> Parser {
  Parser::{ label : label, proc : proc }
}

fn word(w : String) -> Parser {
  fn parse(ps : ParseState) -> Result[Unit, ParseError] {
    let loc = ps.cur()
    for i = 0; i < w.length(); i = i + 1 {
      match ps.eat(w[i]) {
        Ok(_) => ()
        Err(e) => return Err(ParseError((Recoverable, "\(loc), ")))
      }
    } else {
      Ok(())
    }
  }
  Parser::new("word(\"\(w)\")", parse)
}