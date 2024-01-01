{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs = { self, nixpkgs }: 
  let 
     system = "x86_64-linux";
     pkgs' = nixpkgs.legacyPackages.${system};
     out = pkgs'.haskellPackages.callCabal2nix "48-scheme" "${self}" { };
   in {
    devShells.${system}.default =
      pkgs'.mkShell {
        buildInputs = [
          pkgs'.ghc
	  pkgs'.gitMinimal # need this or cabal may fail. annoying.
	  pkgs'.cabal-install
	  pkgs'.cabal2nix
	  pkgs'.haskell-language-server
	];
      };
    packages.${system}.default = out;
    apps.${system}.default = {
      type = "app";
      program =  "${out}/bin/x48-scheme-mrr";
    };
  };
}
