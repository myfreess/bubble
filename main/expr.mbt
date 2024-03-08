enum Expr {
  Fn(Loc, Var, Expr)
  App(Loc, Expr, Expr)
  FnType(Loc, Param[Expr], Expr)
  Univ(Loc)
  UnResolved(Loc, Var)
  Resolved(Loc, Var)
} derive (Debug)

fn to_string(self : Expr) -> String {
  match self {
    Fn(_, var, expr) => "Fn(\(var), \(expr))"
    App(_, f, x) => "App(\(f), \(x))"
    FnType(_, param, exp) => "FnType(\(param), \(exp))"
    Univ(_) => "Univ"
    UnResolved(_, var) => "Unresolved(\(var))"
    Resolved(_, var) => "Resolved(\(var))"
  }
}