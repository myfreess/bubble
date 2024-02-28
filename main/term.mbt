enum Term {
  Refer(Var)
  Univ
  FnType(Param[Term], Term)
  Fn(Param[Term], Term)
  App(Term, Term)
}