type source =
  | SymbolTables
  | StackFrames
  | SemanticAnalysis
  [@@deriving eq, show]

type t =
  { sources: source list;
    enabled: bool
  }
