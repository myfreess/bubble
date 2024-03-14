type ScopeError String derive(Debug, Show)

struct Resolver {
  m : RHTable[String, Var]
  names : RHSet[String]
}

fn resolve_defs(self : Resolver, ds : Array[Def[Expr]]) -> Result[Array[Def[Expr]], ScopeError] {
  let arr = Array::make(ds.length(), Def::default())
  for i = 0; i < ds.length(); i = i + 1 {
    let d = self.resolve_def(ds[i])?
    arr[i] = d
  } else {
    Ok(arr)
  }
}

fn resolve_def(self : Resolver, d : Def[Expr]) -> Result[Def[Expr], ScopeError] {
  let mut shadowed = List::Nil // 被覆盖 (shadowed) 的变量, 需要在检查作用域后恢复
  let mut fresh = List::Nil // 没有被覆盖的全新的变量, 需要在检查作用域后删除
  let mut params = List::Nil // 检查完毕的参数列表
  loop d.params {
    Nil => ()
    Cons(p, ps) => {
      let old = self.insert(p.name)
      match old {
        None => fresh = Cons(p.name, fresh)
        Some(old) => shadowed = Cons(old, shadowed)
      }
      let typ = self.resolve_expr(p.typ)?
      let param = Param::{ name : p.name, typ : typ }
      params = Cons(param, params)
      continue(ps)
    }
  }
  let ret = self.resolve_expr(d.ret)?
  let body = self.resolve_expr(d.body)?
  loop fresh {
    Nil => ()
    Cons(v, vs) => {
      self.m.delete(v.text) |> ignore
      continue(vs)
    }
  }
  loop shadowed {
    Nil => ()
    Cons(v, vs) => {
      self.insert(v) |> ignore
      continue(vs)
    }
  }
  // 检查定义是否重名
  if self.names.contains(d.name.text) {
    let loc = d.loc
    let name = d.name.text
    Err(ScopeError("\(loc): duplicate name \(name)"))
  } else {
    self.names.add(d.name.text)
    // 插入新的定义, 后续定义可以引用这个全局定义
    self.insert(d.name) |> ignore
    Ok(Def::{ loc : d.loc, name : d.name, params : reverse(params), ret : ret, body : body })
  }
}

fn resolve_expr(self : Resolver, e : Expr) -> Result[Expr, ScopeError] {
  match e {
    UnResolved(loc, var) => {
      let name = var.text
      match self.m[name] {
        None => Err(ScopeError("\(loc): unresolved variable \(name)"))
        Some(var) => Ok(Resolved(loc, var))
      }
   }
    Fn(loc, v, body) => {
      let b = self.guard(v, body)?
      Ok(Fn(loc, v, b))
    }
    App(loc, f, x) => {
      let f = self.resolve_expr(f)?
      let x = self.resolve_expr(x)?
      Ok(App(loc, f, x))
    }
    FnType(loc, p, body) => {
      let typ = self.resolve_expr(p.typ)?
      // body 中能够引用变量 p.name
      let b = self.guard(p.name, body)?
      Ok(FnType(loc, Param::{ name : p.name, typ : typ }, b))
    }
    Univ(_) => {
      Ok(e)
    }
    other => abort("resolve_expr(): impossible case \(other)")
  }
}

fn guard(self : Resolver, v : Var, e : Expr) -> Result[Expr, ScopeError] {
  // 在 v 的保护下 (即插入 v 到上下文中, 检查完毕后删除), 检查表达式 e 的作用域
  let old = self.insert(v)
  let ret = self.resolve_expr(e)?
  match old {
    None => self.m.delete(v.text) |> ignore
    Some(old) => self.insert(old) |> ignore
  }
  return Ok(ret)
}

fn insert(self : Resolver, v : Var) -> Option[Var] {
  // 插入一个新的变量到上下文中. 返回旧的变量 (如果存在的话)
  let old = self.m[v.text]
  self.m[v.text] = v
  return old
}