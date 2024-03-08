// EBNF 规则:
//
//    prog = defn*
//
//    defn = 'fn' ident param* '->' expr '{' expr '}'
//
//    param = '(' ident ':' expr ')'
//
//    expr = '|' ident '|' '{' expr '}'   # fn
//         | param '->' expr              # fn_type
//         | 'type'                       # univ
//         | primary_expr expr            # app
//         | ident                        # ref
//         | '(' expr ')'                 # paren_expr
//
//    primary_expr = ref
//                 | paren_exp


enum Token {
  Arrow // "->"
  Identifier(String) // lower_case风格
  Open(Char) // '(' '{'
  Close(Char) // ')' '}'
  Sep // '|'
  Colon // ':'
  FN // fn
  TYPE // type
  EOF
} derive(Eq, Debug, Show)

fn in(this : Char, lw : Char, up : Char) -> Bool {
  this >= lw && this <= up
}

fn isDigit(this : Char) -> Bool {
  this |>  in('0', '9')
}

fn isLower(this : Char) -> Bool {
   this |> in('a', 'z')
}

fn isIdChar(this : Char) -> Bool {
  isLower(this) || isDigit(this) || this == '_' 
}

fn isWhiteSpace(this : Char) -> Bool {
  this == ' ' || this == '\t'
}

fn isNewline(this : Char) -> Bool {
  this == '\n'
}

fn isOpen(this : Char) -> Bool {
  this == '(' || this == '{'
}

fn isClose(this : Char) -> Bool {
  this == ')' || this == '}'
}

fn isColon(this : Char) -> Bool {
  this == ':'
}

fn isSep(this : Char) -> Bool {
  this == '|'
}

struct Tokens {
  ids : IDs
  contents : Array[(Loc, Token)]
  mut current : Int
}

fn Tokens::newWith(contents : Array[(Loc, Token)], ids : IDs) -> Tokens {
  Tokens::{ ids : ids, contents : contents, current : 0 }
}

fn Tokens::new(source : String, ids : IDs) -> Tokens {
  let mut pos = 0
  let mut column = 1
  let mut line = 1
  fn next_column() {
    pos = pos + 1
    column = column + 1
  }
  let mut lst_len = 0
  fn tokenize() -> List[(Loc, Token)] {
    if pos < source.length() {
      let ch = source[pos]
      if isWhiteSpace(ch) {
        // 跳过空白字符
        next_column()
        tokenize()
      } else if isNewline(ch) {
        pos = pos + 1
        column = 1 // 重置
        line = line + 1
        tokenize()
      } else if isLower(ch) {
        // 可能是Identifier || FN || TYPE
        let loc = Loc::new(pos, line, column)
        let buf = Buffer::make(42)
        buf.write_char(ch)
        next_column()
        while pos < source.length() && isIdChar(source[pos]) {
          buf.write_char(source[pos])
          next_column()
        }
        let str = buf.to_string()
        lst_len = lst_len + 1
        if str == "fn" {
          Cons((loc, FN), tokenize())
        } else if str == "type" {
          Cons((loc, TYPE), tokenize())
        } else {
          Cons((loc, Identifier(str)), tokenize())
        }
      } else if isColon(ch) {
        next_column()
        let loc = Loc::new(pos, line, column)
        lst_len = lst_len + 1
        Cons((loc, Colon), tokenize())
      } else if isSep(ch) {
        next_column()
        let loc = Loc::new(pos, line, column)
        lst_len = lst_len + 1
        Cons((loc, Sep), tokenize())
      } else if ch == '-' {
        next_column()
        let ch = source[pos]
        if ch == '>' {
          next_column()
          let loc = Loc::new(pos, line, column)
          lst_len = lst_len + 1
          Cons((loc, Arrow), tokenize())
        } else {
          abort("Tokens::new(): line \(line) column \(column), expect \'>\' but got \(ch)")
        }
      } else if isClose(ch) {
        next_column()
        let loc = Loc::new(pos, line, column)
        lst_len = lst_len + 1
        Cons((loc, Close(ch)), tokenize())
      } else if isOpen(ch) {
        next_column()
        let loc = Loc::new(pos, line, column)
        lst_len = lst_len + 1
        Cons((loc, Open(ch)), tokenize())
      } else {
        abort("Tokens::new(): cant recognize \(ch)")
      }
    } else {
      Nil
    }
  }
  let tok_list = tokenize()
  let arr = Array::make(lst_len, (Loc::new(-1, -1, -1), Token::EOF))
  let mut i = 0
  loop tok_list {
    Cons(t, ts) => {
      arr[i] = t
      i = i + 1
      continue(ts)
    }
    Nil => ()
  }
  Tokens::newWith(arr, ids)
}

fn peek(self : Tokens) -> Token {
  if self.current < self.contents.length() {
    let (_, tok) = self.contents[self.current]
    return tok
  } else {
    return EOF
  }
}

fn cur(self : Tokens) -> Loc {
  let (loc,_) = self.contents[self.current]
  loc
}

fn next(self : Tokens) {
  self.current = self.current + 1
}

struct ParseError {
  loc : Loc
  msg : String
} derive (Debug, Show)

fn eat(self : Tokens, tok : Token) -> Result[Unit, ParseError] {
  let got = self.peek()
  if tok == got {
    self.next()
    Ok(())
  } else {
    let loc = self.cur()
    Err(ParseError::{ loc : loc, msg : "Tokens::eat(): expect \(tok) but got \(got)" })
  }
}

fn choice(self : Tokens, ps : Array[(Tokens) -> Result[Expr, ParseError]]) -> Result[Expr, ParseError] {
  let current = self.current
  let mut res = (ps[0])(self)
  match res {
    Ok(_) => return res
    Err(_) => self.current = current // 恢复到原位置
  }
  for i = 1; i < ps.length(); i = i + 1 {
    let current = self.current
    let r = (ps[i])(self)
    match r {
      Ok(_) => return r
      Err(_) => {
        self.current = current // 恢复到原位置
        res = r 
      }
    }
  } else {
    return res
  }
}

fn many[T : Show](self : Tokens, p : (Tokens) -> Result[T, ParseError]) -> List[T] {
  fn go() -> List[T] {
    let res = p(self)
    match res {
      Ok(res) => Cons(res, go())
      Err(_) => {
        Nil
      }
    }
  }
  let lst = go()
  lst
}

fn parseDefn(self : Tokens) -> Result[Def[Expr], ParseError] { 
  let loc = self.cur()
  self.eat(FN)?
  let name = self.parseIdentifier()?
  let params = self.many(parseParam)
  self.eat(Arrow)?
  let ret = self.parseExpr()?
  self.eat(Open('{'))?
  let body = self.parseExpr()?
  self.eat(Close('}'))?
  Ok(Def::{ loc : loc, name : name, params : params, ret : ret, body : body })
}

fn parseExpr(self : Tokens) -> Result[Expr, ParseError] {
  self.choice([parseFN, parseFNTYPE, parseUniv, parseApp, parseRef, parseParenExpr])
}

fn parseParam(self : Tokens) -> Result[Param[Expr], ParseError] {
  self.eat(Open('('))?
  let var = self.parseIdentifier()?
  self.eat(Colon)?
  let expr = self.parseExpr()?
  self.eat(Close(')'))?
  Ok(Param::{ name : var, typ : expr })
}


fn parseIdentifier(self : Tokens) -> Result[Var, ParseError]{
  let tok = self.peek()
  match tok {
    Identifier(s) => {
      self.next()
      let unique = self.ids.fresh()
      Ok(Var::{ text : s, id : unique })
    }
    other => {
      let loc = self.cur()
      Err(ParseError::{ loc : loc, msg : "expect an identifer but got \(other)" })
    }
  }
}

fn parseRef(self : Tokens) -> Result[Expr, ParseError] {
  let loc = self.cur()
  let var = parseIdentifier(self)?
  Ok(UnResolved(loc, var))
}

fn parseParenExpr(self : Tokens) -> Result[Expr, ParseError] {
  self.eat(Open('('))?
  let expr = self.parseExpr()?
  self.eat(Close(')'))?
  return Ok(expr)
}

fn parsePrimary(self : Tokens) -> Result[Expr, ParseError] {
  self.choice([parseRef, parseParenExpr])
}

fn parseFN(self : Tokens) -> Result[Expr, ParseError] {
  // 此处仅处理lambda
  let loc = self.cur()
  self.eat(Sep)?
  let var = self.parseIdentifier()?
  self.eat(Sep)?
  self.eat(Open('{'))?
  let body = self.parseExpr()?
  self.eat(Close('}'))?
  Ok(Fn(loc, var, body))
}

fn parseApp(self : Tokens) -> Result[Expr, ParseError] {
  let loc = self.cur()
  let f = self.parsePrimary()?
  let x = self.parseExpr()?
  Ok(App(loc, f, x))
}

fn parseFNTYPE(self : Tokens) -> Result[Expr, ParseError] {
  let loc = self.cur()
  let p = self.parseParam()?
  let body = self.parseExpr()?
  Ok(FnType(loc, p, body))
}

fn parseUniv(self : Tokens) -> Result[Expr, ParseError] {
  let loc = self.cur()
  self.eat(TYPE)?
  Ok(Univ(loc))
}