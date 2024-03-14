struct Normalizer {
  ids : IDs
  env : RHTable[ID, Term]
}

fn Normalizer::new(ids : IDs) -> Normalizer {
  Normalizer::{ ids : ids, env : RHTable::new(42) }
}

fn term(self : Normalizer, tm : Term) -> Term {
  match tm {
    Refer(v) =>
      match self.env[v.id] {
        None => tm
        // 进行变量替换前需要rename
        // 一个因为不rename发生变量捕获的例子：
        // def Foo#1(x#2) { #2 }
        // def Bar#3() { Foo#1 }
        // def XXX#4() { Foo#1 Bar#3 }
        // ======================================================
        // def Bar#3() { lambda x#2 : x#2 }
        // ======================================================
        // def XXX#4() { (lambda x#2 : x#2) (lambda x#2 : x#2) }
        Some(tm) => self.term(Renamer::new(self.ids).rename(tm))
      }
    App(f, x) => {
      let f = self.term(f)
      let x = self.term(x)
      match f {
        Fn(p, b) => self.subst((p.name, x), b)
        _ => App(f, x)
      }
    }
    Fn(p, b) => Fn(self.param(p), self.term(b))
    FnType(p, b) => FnType(self.param(p), self.term(b))
    Univ => tm 
  }
}

fn param(self : Normalizer, p : Param[Term]) -> Param[Term] {
  Param::{ name : p.name, typ : self.term(p.typ) }
}

fn subst(self : Normalizer, pair : (Var, Term), tm : Term) -> Term {
  let (v, x) = pair
  self.env[v.id] = x
  self.term(tm)
}

fn apply(self : Normalizer, f : Term, arg : Term) -> Term {
  match f {
    Fn(p, b) => self.subst((p.name, arg), b)
    _ => App(f, arg)
  }
}

fn to_value(d : Def[Term]) -> Term {
  let mut ret = d.body
  loop d.params {
    Nil => ()
    Cons(p, ps) => {
      ret = Fn(p, ret)
      continue(ps)
    }
  }
  return ret
}

fn to_typ(d : Def[Term]) -> Term {
  let mut ret = d.ret
  loop d.params {
    Nil => ()
    Cons(p, ps) => { 
      ret = FnType(p, ret)
      continue(ps)
    }
  }
  return ret
}