struct Unifier {
  ids : IDs
  globals : RHTable[ID, Def[Term]]
}

fn Unifier::new(ids : IDs, globals : RHTable[ID, Def[Term]]) -> Unifier {
  Unifier::{ ids : ids, globals : globals }
}

// 只做相等检查
fn unify(self : Unifier, lhs : Term, rhs : Term) -> Bool {
  match (lhs, rhs) {
    (Refer(x), Refer(y)) => x.text == y.text && x.id == y.id
    (App(f, x), App(g, y)) => self.unify(f, g) && self.unify(x, y)
    (Fn(p, b), Fn(q, c)) => self.unify(b, self.nf().subst((q.name, Refer(p.name)), c))
    (FnType(p, b), FnType(q, c)) => 
      if not(self.unify(p.typ, q.typ)) {
        false
      } else {
        self.unify(b, self.nf().subst((q.name, Refer(p.name)), c))
      }
    (Univ, Univ) => true
    _ => false
  }
}

fn nf(self : Unifier) -> Normalizer {
  Normalizer::new(self.ids)
}