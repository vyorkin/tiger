type t = {
  trace_sources: Trace.source list;
}

let cfg = ref {
    trace_sources = []
  }

let setup cfg =
  ()
