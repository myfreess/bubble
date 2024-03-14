type TyckError String derive (Debug, Show)

struct Elaborator {
  ids : IDs
  globals : RHTable[ID, Def[Term]]
  locals : RHTable[ID, Term]  
}

fn Elaborator::new(ids : IDs) -> Elaborator {
  Elaborator::{ ids : ids, globals : RHTable::new(42), locals : RHTable::new(42) }
}

fn elaborate(self : Elaborator, ds : Array[Def[Expr]]) -> Result[Array[Def[Term]], TyckError] {
  let arr = Array::make(ds.length(), Def::default())
  for i = 0; i < ds.length(); i = i + 1 {
    let d = self.elaborate_def(ds[i])?
    arr[i] = d
  } else {
    Ok(arr)
  }
}

fn elaborate_def(self : Elaborator, d : Def[Expr]) -> Result[Def[Term], TyckError] {
  let mut checked = List::Nil // 已经检查过的局部变量
  let mut ps = List::Nil // 已经检查过的函数参数
  loop d.params {
    Nil => ()
    Cons(p, rest) => {
      let typ = self.check(p.typ, Univ)?
      ps = Cons(Param::{ name : p.name, typ : typ }, ps)
      self.locals[p.name.id] = typ // 加入到局部变量中
      checked = Cons(p.name.id, checked)
      continue(rest)
    }
  }
  let ret = self.check(d.ret, Univ)?
  let body = self.check(d.body, ret)?
  // 清空局部变量, 下一个定义的检查用不到了
  loop checked {
    Nil => ()
    Cons(v, rest) => { 
      self.locals.delete(v) |> ignore
      continue(rest)
    }
  }
  let checked_def = Def::{ loc : d.loc, name : d.name, params : reverse(ps), ret : ret, body : body}
  self.globals[d.name.id] = checked_def // 将此定义加入到全局中
  Ok(checked_def)
}

fn check(self : Elaborator, e : Expr, typ : Term) -> Result[Term, TyckError] {
  match e {
    Fn(loc, v, body) => {
      //        Γ , x : A ⊢ M : B
      // --------------------------------- function introduction rule
      // Γ ⊢ λ (x : A) → M : π (x : A) → B
      match self.nf().term(typ) {
        FnType(p, b) => {
          let body_type = self.nf().subst((p.name, Refer(v)), b)
          let param = Param::{ name : v, typ : p.typ }
          let body = self.guarded_check(param, body, body_type)?
          Ok(Fn(param, body))
        }
        typ => Err(TyckError("\(loc): expected '\(typ)', got function type"))
      }
    }
    _ => {
      // 其余的表达式进行类型推导, 用推导的类型和期盼的类型判断是否一致
      let (tm, got) = self.infer(e)?
      let got = self.nf().term(got)
      let typ = self.nf().term(typ)
      if self.unify(got, typ) {
        Ok(tm)
      } else {
        let loc = e.loc()   
        Err(TyckError("\(loc): expected '\(typ)', got '\(got)'"))
      }
    }
  }
}

fn infer(self : Elaborator, e : Expr) -> Result[(Term, Term), TyckError] {
  match e {
    Resolved(loc, v) => {
      match self.locals[v.id] {
        None => {
          match self.globals[v.id] {
            Some(d) => {
              // 将全局定义转换成对应的值和类型, 并且刷新内部的变量引用.
              let value = self.rename(to_value(d))
              let typ = self.rename(to_typ(d))
              Ok((value, typ))
            }
            None => abort("impossible")
            // 由于提前做过作用域检查, 所以不可能在本地和全局都不存在
          }
        }
        Some(tm) => Ok((Refer(v), tm))
      }
    }
    FnType(_, p, b) => {
      //   Γ , A : type ⊢ M : type
      // -------------------------- function type introduction rule
      // Γ ⊢ π (A: type) → M : type
      let (p_typ, _) = self.infer(p.typ)? // 参数类型的类型一定是 Univ, 所以忽略了
      let infered_p = Param::{ name : p.name, typ : p_typ }
      // 在参数 p 的保护下, 推导 body 的类型
      let (b_tm, b_ty) = self.guarded_infer(infered_p, b)?
      Ok((FnType(infered_p, b_tm), b_ty))
    }
    App(_, f, x) => {
      // Γ ⊢ f : π (x : A) → B    x : A
      // ------------------------------ function elimination rule
      //          Γ ⊢ f x : B
      let (f_tm, f_typ) = self.infer(f)?
      match f_typ {
        FnType(p, b) => {
          // 参数 p 的保护下检查参数 x 的类型必须是函数的参数 p 的类型
          let x_tm = self.guarded_check(p, x, p.typ)?
          // 表达式的类型即 b, 但是要将 b 内的 p 替换成 x
          let typ = self.nf().subst((p.name, x_tm), b)
          // 尝试对表达式进行计算
          let tm = self.nf().apply(f_tm, x_tm)
          Ok((tm, typ))
        }
        typ => {
          let loc = f.loc()
          Err(TyckError("\(loc): expected function type, got \(typ)"))
        }
      }
    }
    Univ(_) => Ok((Univ, Univ))
    // universe introduction rule
    _ => abort("impossible")
  }
}

fn guarded_check(self : Elaborator, p : Param[Term], e : Expr, typ : Term) -> Result[Term, TyckError] {
  // 在 p 的保护下 (即将 p 加入到本地变量中, 检查完毕后删除), 检查表达式 e 的类型是否为 typ
  self.locals[p.name.id] = p.typ
  let ret = self.check(e, typ)?
  self.locals.delete(p.name.id) |> ignore
  Ok(ret)
}

fn guarded_infer(self : Elaborator, p : Param[Term], e : Expr) -> Result[(Term, Term), TyckError] {
  // 在 p 的保护下 (即将 p 加入到本地变量中, 推导完毕后删除), 推导表达式 e 的类型
  self.locals[p.name.id] = p.typ
  let ret = self.infer(e)?
  self.locals.delete(p.name.id) |> ignore
  Ok(ret)
}

fn nf(self : Elaborator) -> Normalizer {
  Normalizer::new(self.ids)
}

fn unify(self : Elaborator, lhs : Term, rhs : Term) -> Bool {
  Unifier::new(self.ids, self.globals).unify(lhs, rhs)
}

fn rename(self : Elaborator, tm : Term) -> Term {
  Renamer::new(self.ids).rename(tm)
}