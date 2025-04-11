{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
      in {
        formatter = pkgs.nixfmt-rfc-style;
        devShells.default = pkgs.mkShell {
          name = "dap-tui-dev";
          buildInputs = with pkgs; [
            nix-init
            curl
            jq
            (lua5_3_compat.withPackages (
              ps:
                with ps; let
                  luasystem-0-5-1 = buildLuarocksPackage {
                    pname = "luasystem";
                    version = "0.5.1";

                    buildInputs = [glibc];
                    src = fetchFromGitHub {
                      owner = "lunarmodules";
                      repo = "luasystem";
                      rev = "v0.5.1";
                      hash = "sha256-+dkXf4F2mZgQSRQRpJnjwo5Swi06Pi2BQjjY5p3PQGc=";
                    };
                    knownRockspec = fetchurl {
                      url = "https://luarocks.org/manifests/lunarmodules/luasystem-0.5.1-1.rockspec";
                      hash = "sha256-wES/1xAro+rsRAI7H9MHD+MXt5nKYNr7GzTxN9TYKio=";
                    };
                  };
                  terminal-lua = buildLuarocksPackage {
                    pname = "terminal";
                    version = "unstable-2025-03-31";

                    nativeBuildInputs = [luatutf8];
                    propagatedBuildInputs = [
                      curl
                      lua
                      luautf8
                      luasystem-0-5-1
                    ];
                    src = fetchFromGitHub {
                      owner = "lunarmodules";
                      repo = "terminal.lua";
                      rev = "1143b70c0c419103ab6504df6fbf4a5768250673";
                      hash = "sha256-2qZIohCZ4Sr7xhU+3x4R5fZIvLaEvTENa5lmGGmizwk=";
                    };
                    knownRockspec = "terminal-scm-1.rockspec";
                  };
                  luasocket-3-0rc = buildLuarocksPackage rec {
                    pname = "luasocket";
                    version = "3.0rc1";
                    propagatedBuildInputs = [lua];
                    src = fetchFromGitHub {
                      owner = "lunarmodules";
                      repo = "luasocket";
                      rev = "v${version}";
                      hash = "sha256-s+sPg/PEI8VckxcmE/gb6bjF4wfm59PJZHTEoc4/GrI=";
                    };
                    knownRockspec = fetchurl {
                      url = "https://luarocks.org/manifests/lunarmodules/luasocket-3.0rc1-2.rockspec";
                      hash = "sha256-TBAn6ApX0vXIo1TEC1dx9wwJn23o98d52dd/cmGcy50=";
                    };
                  };
                  timerwheel = buildLuarocksPackage rec {
                    pname = "timerwheel";
                    version = "1.0.2";

                    propagatedBuildInputs = [coxpcall];

                    src = fetchFromGitHub {
                      owner = "Tieske";
                      repo = "timerwheel.lua";
                      rev = version;
                      hash = "sha256-qgZPUmdcSfnf9tTA+kL2V05WoqVBCsqAghTpiqHlpwI=";
                    };
                    knownRockspec = fetchurl {
                      url = "https://luarocks.org/manifests/tieske/timerwheel-1.0.2-1.rockspec";
                      hash = "sha256-4b4/WE4/LGzrvjDI69aZDseAO8SPEWPY1NCAFu2es0Q=";
                    };
                  };
                  copas = buildLuarocksPackage rec {
                    pname = "copas";
                    version = "4.8.0";

                    propagatedBuildInputs = [
                      binaryheap
                      coxpcall
                      timerwheel
                      luasocket-3-0rc
                      luasystem-0-5-1
                    ];
                    src = fetchFromGitHub {
                      owner = "lunarmodules";
                      repo = "copas";
                      rev = "v4_8_0";
                      hash = "sha256-7F7WHNAbfcWZfyC+kB/NnhZWKUAef4a6FZZE3Pat1Aw=";
                    };
                    knownRockspec = fetchurl {
                      url = "https://luarocks.org/manifests/tieske/copas-4.8.0-1.rockspec";
                      hash = "sha256-OOBe5j9sE8c5YBa9J9J1Qu5C+U8/A0WCS8K/OAGOUC0=";
                    };
                  };
                in [
                  coxpcall
                  cjson
                  copas
                  fennel
                  inspect
                  luautf8
                  penlight
                  terminal-lua
                ]
            ))
          ];
          shellHook = "";
        };
      }
    );
}
