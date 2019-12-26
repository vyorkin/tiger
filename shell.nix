with import <nixpkgs> { };

pkgs.mkShell {
  buildInputs = [
    m4 cmake opam gcc ncurses boost openssl
    zlib gmp procps secp256k1 libffi pkgconfig pcre patdiff
  ];
}
