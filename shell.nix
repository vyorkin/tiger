with import <nixpkgs> { };

let
  ocamlVersion = (builtins.parseDrvName pkgs.ocamlPackages.ocaml.name).version;
  packages = (with pkgs.ocamlPackages; [
    ocaml
    base
    re
    re2
    fieldslib
    ppx_fields_conv
    ppx_let
    ppx_import
    ppx_deriving
    ppx_sexp_conv
    ppx_inline_test
    ppx_expect
    menhir
    stdio
    core
    core_extended
    core_bench
    js_of_ocaml
    js_of_ocaml-compiler
    js_of_ocaml-ppx
    js_of_ocaml-ocamlbuild

    findlib
    utop
    merlin
    ounit
    ocp-indent
    ocp-index

    async
    yojson
    cohttp
    # async_graphics
    cryptokit
  ]);
  mkpath = p: "${p}/lib/ocaml/${ocamlVersion}/site-lib";
  paths = builtins.concatStringsSep ":" (map mkpath packages);
in pkgs.mkShell {
  buildInputs = with pkgs; [ dune ncurses ] ++ packages;

  shellHook = ''
    export CAML_LD_LIBRARY_PATH="$CAML_LD_LIBRARY_PATH:${paths}"
  '';
}
