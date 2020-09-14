let
  tooling = import ./default.nix;
  self = tooling.purescript;
in
  tooling.pkgs.mkShell {
      buildInputs = [
        self.nodejs
        self.purs
        self.spago
        self.pscid
        self.spago2nix
        tooling.pkgs.postgresql
      ];
  }
