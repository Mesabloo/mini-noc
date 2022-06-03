{ pkgs ? import ./nixpkgs.nix }:

# let
#   ghc-dev = pkgs.haskell.compiler.ghcHEAD.override {
#     ghcFlavour = "devel2";
#   };

#   ghc-dev' = ghc-dev.overrideAttrs
#     (old: {
#       version = "git-head";
#       src = pkgs.fetchgit {
#         url = "https://gitlab.haskell.org/ghc/ghc.git/";
#         rev = "9fa790b4b33fe75c86ed7a3032eecd35774eb21e";
#         sha256 = "14gdg1x7p1fanzpa5rin5gklc2ls865j7xrddgsry6wsvb7105y9";
#       };

#       # For some reason, my GCC installation does not come with a 'cxx' executable.
#       # A workaround is to use 'cpp' instead.
#       preConfigure = builtins.replaceStrings [ "/cxx" ] [ "/cpp" ] old.preConfigure;
#     });

# in
pkgs.mkShell {
  name = "mini-noc";

  # nativeBuildInputs = with pkgs; [
  #   gcc
  #   cabal-install
  # ];

  buildInputs = with pkgs; [
    haskell.compiler.ghcHEAD
    gnumake
    rr
  ];
}

