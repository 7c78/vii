{
    inputs = {
        nixpkgs = {
            url = "github:NixOS/nixpkgs/nixos-unstable";
        };
        all-cabal-hashes = {
            url = "github:commercialhaskell/all-cabal-hashes/hackage";
            flake = false;
        };
    };

    outputs = { self, nixpkgs, all-cabal-hashes }:
        let packageName = "vii";
            system = "x86_64-linux";
            pkgs = import nixpkgs {
                inherit system;

                overlays = [
                    (self: super: {
                        inherit all-cabal-hashes;

                        haskellPackagesCustom = self.haskellPackages.override (old: {
                            overrides =
                                let hlib = self.haskell.lib.compose;
                                 in self.lib.composeManyExtensions [
                                        (hlib.packageSourceOverrides {
                                            ${packageName} = ./.;
                                        })
                                    ];
                        });
                    })
                ];
            };
            hsPkgs = pkgs.haskellPackagesCustom;
        in {
            devShells.${system}.default = hsPkgs.shellFor {
                withHoogle = true;
                packages = p: [ p.${packageName} ];

                nativeBuildInputs = [
                    pkgs.ghciwatch
                    hsPkgs.cabal-install

                    (pkgs.writeShellScriptBin "b" ''
                        cabal build
                    '')

                    # test [pattern]
                    (pkgs.writeShellScriptBin "t" ''
                        if [ "$#" -eq 0 ]; then
                            cabal test --test-show-details=direct
                        else
                            cabal test --test-show-details=direct --test-option=--match --test-option="$*"
                        fi
                    '')

                    (pkgs.writeShellScriptBin "i" ''
                        case "$1" in
                            "app")
                                cabal repl exe:${packageName}
                                ;;
                            "test")
                                cabal repl test:${packageName}-test
                                ;;
                            *)
                                cabal repl lib:${packageName}
                                ;;
                        esac
                    '')

                    (pkgs.writeShellScriptBin "d" ''
                        case "$1" in
                            "app")
                                ghciwatch --command="cabal repl exe:${packageName} --flag=dev" --watch=app
                                ;;
                            "test")
                                ghciwatch --command="cabal repl test:${packageName}-test --flag=dev" --watch=test
                                ;;
                            *)
                                ghciwatch --command="cabal repl lib:${packageName} --flag=dev" --watch=src
                                ;;
                        esac
                    '')

                    (pkgs.writeShellScriptBin "x" ''
                        if [ "$#" -eq 0 ]; then
                            cabal run exe:${packageName}
                        else
                            cabal run exe:${packageName} -- "$@"
                        fi
                    '')

                    (pkgs.writeShellScriptBin "h" ''
                        hoogle server --port=8080 --local --haskell
                    '')
                ];
            };
        };
}
