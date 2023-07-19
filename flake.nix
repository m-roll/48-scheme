{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }: {
    devShell."x86_64-linux" =
      with import nixpkgs { system = "x86_64-linux"; };
      pkgs.mkShell {
        buildInputs = [
          ghc
	  git # need this or cabal may fail. annoying.
	  cabal-install
	  cabal2nix
	];
      };
  };
}
