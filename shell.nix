with import <nixpkgs> { };

let siteLisp = "share/emacs/site-lisp";
in pkgs.mkShell {
  buildInputs = with pkgs;
  [dune ncurses] ++ (with ocamlPackages; [
    ocaml
    base
    stdio
    core
    core_extended
    core_bench
    findlib
    utop
    merlin
    ocp-indent
    ocp-index
  ]);
}
