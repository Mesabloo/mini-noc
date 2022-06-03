import
  (builtins.fetchTarball {
    name = "nixpkgs-pinned";
    url = "https://github.com/nixos/nixpkgs/archive/f1c9c23aad972787f00f175651e4cb0d7c7fd5ea.tar.gz";
    # Use `nix-prefetch-url --unpack <url>`
    sha256 = "01msl5fffdrg7q43amvaqv9m00c2j1y3ihz3xhhnwl56l46df05j";
  })
{ }
