{
  description = "Environment setup for AOC 2024 with Lean 4";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = {nixpkgs, ...} @ input: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = pkgs.mkShell {
      name = "advent-lean4-2024";
      buildInputs = with pkgs; [
        elan
      ];
    };
  };
}
