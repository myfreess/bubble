// common.mbt 放一些都能用到的零散定义


struct Loc {
  mut pos : Int // position
  mut ln : Int // line
  mut col : Int // column
}

fn Loc::new() -> Loc {
  Loc::{ pos : 0, ln : 1, col : 1 }
}

fn copy(self : Loc) -> Loc {
  Loc::{ pos : self.pos, ln : self.ln, col : self.col }
}

fn next_line(self : Loc) {
  self.pos = self.pos + 1
  self.ln = self.ln + 1
  self.col = 1
}

fn next_column(self : Loc) {
  self.pos = self.pos + 1
  self.col = self.col + 1
}

fn to_string(self : Loc) -> String {
  let line = self.ln
  let column = self.col
  "\(line):\(column)"
}

type ID Int derive(Eq)

fn hash(self : ID) -> Int {
  let ID(n) = self
  n.hash()
}

struct Var {
  text : String
  id : ID
}

fn to_string(self : Var) -> String {
  self.text
}

struct IDs {
  mut counter : Int
}

fn IDs::new() -> IDs {
  IDs::{ counter : 0 }
}

fn fresh(self : IDs) -> ID {
  let n = self.counter
  self.counter = n + 1
  ID(n)
}

fn rename(self : IDs, v : Var) -> Var {
  Var::{ text : v.text, id : self.fresh() }
}

struct Param[T] {
  name : Var
  typ  : T // Type
}

fn to_string[T : Show](self : Param[T]) -> String {
  let name = self.name
  let typ = self.typ
  "\(name): \(typ)"
}


struct Def[T] {
  // 当 T 是 Expr 时, 它是个尚未通过类型检查的定义 
  // 如果是 Term, 则是个类型安全 (well-typed)的定义
  loc : Loc
  name : Var
  params : List[Param[T]]
  ret : T
  body : T
}

fn to_string[T : Show](self : Def[T]) -> String {
  let params = Buffer::make(20)
  loop self.params {
    Nil => ()
    Cons(p, ps) => {
      params.write_char(' ')
      params.write_string(p.to_string())
      continue(ps)
    }
  }
  let params = params.to_string()
  let fnname = self.name
  let ret = self.ret
  let body = self.body
  "fn \(fnname)\(params) -> \(ret) { \n\t \(body) \n\t }"
}