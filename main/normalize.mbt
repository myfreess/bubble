struct Normalizer {
  ids : IDs
  globals : RHTable[ID, Def[Term]]
  env : RHTable[ID, Term]
}

fn Normalizer::new(ids : IDs, globals : RHTable[ID, Def[Term]]) -> Normalizer {
  Normalizer::{ ids : ids, globals : globals, env : RHTable::new(42) }
}

fn term(self : Normalizer, tm : Term) -> Term {
  match tm {
    Refer(v) =>
      match self.env[v.id] {
        None => tm
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

fn to_value(self : Normalizer, d : Def[Term]) -> Term {
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

fn to_typ(self : Normalizer, d : Def[Term]) -> Term {
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