

struct Elaborator {
  ids : IDs
  globals : RHTable[ID, Def[Term]]
  locals : RHTable[ID, Term]
}

fn Elaborator::new(ids : IDs) -> Elaborator {
  Elaborator::{ ids : ids, globals : RHTable::new(42), locals : RHTable::new(42) }
}

