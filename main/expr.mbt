enum Expr {
  Fn(Loc, Var, Expr)
  App(Loc, Expr, Expr)
  FnType(Loc, Param[Expr], Expr)
  Univ(Loc)
  UnResolved(Loc, Var)
  Resolved(Loc, Var)
} derive (Debug)

fn Expr::default() -> Expr {
  Univ(Loc::new(-1, -1, -1))
}

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

fn loc(self : Expr) -> Loc {
  match self {
    Fn(loc, _, _) => loc
    App(loc, _, _) => loc
    FnType(loc, _, _) => loc
    Univ(loc) => loc
    UnResolved(loc, _) => loc
    Resolved(loc, _) => loc
  }
}