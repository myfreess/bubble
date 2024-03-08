fn test_parser() {
  let ids = IDs::new()
  let tokens = Tokens::new(bubble_add, ids)
  // for i = 0; i < tokens.contents.length(); i = i + 1 {
  //   let (loc, tok) = tokens.contents[i]
  //   print("\(loc) ")
  //   println(tok)
  // }
  let add_defn = tokens.parseDefn()
  match add_defn {
    Err(e) => println(e)
    Ok(r) => println(r)
  }
}


fn init {
  test_parser()
}
