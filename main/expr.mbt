enum Expr {
  Fn(Loc, Var, Expr)
  App(Loc, Expr, Expr)
  FnType(Loc, Param[Expr], Expr)
  Univ(Loc)
  UnResolved(Loc, Var)
  Resolved(Loc, Var)
}