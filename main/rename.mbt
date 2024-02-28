struct Renamer {
  ids : IDs
  m : RHTable[ID, ID]
}

fn Renamer::new(ids : IDs) -> Renamer {
  Renamer::{ ids : ids, m : RHTable::new(42) }
}

fn rename(self : Renamer, tm : Term) -> Term {
  match tm {
    Refer(v) =>
      match self.m[v.id] {
        None => tm
        Some(new_id) => Refer(Var::{ text : v.text, id : new_id })
      }
    App(f, x) => App(self.rename(f), self.rename(x))
    Fn(p, b) => Fn(self.param(p), self.rename(b))
    FnType(p, b) => FnType(self.param(p), self.rename(b))
    Univ => Univ
  }
}

fn param(self : Renamer, p : Param[Term]) -> Param[Term] {
  let name = self.ids.rename(p.name)
  self.m[p.name.id] = name.id
  Param::{ name : name, typ : self.rename(p.typ) }
}