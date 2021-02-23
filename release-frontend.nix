let
  sources = import ./nix/sources.nix { };

  config = import ./config.nix { };
  inherit (config) nixpkgs pursPkgs;

  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreSource;

  spagoPkgs = import ./spago-packages.nix { pkgs = nixpkgs; };

  site-frontend-build =
    nixpkgs.mkYarnPackage rec {
      name = "site-frontend-build";
      src = gitignoreSource ./.;
      packageJSON = ./package.json;
      yarnLock = ./yarn.lock;

      nativeBuildInputs = [
        pursPkgs.purs
        pursPkgs.zephyr
        nixpkgs.nodejs-15_x
      ];

      postBuild = ''
        purs compile --codegen corefn "$src/pf_app/**/*.purs" ${builtins.toString
          (builtins.map
            (x: ''"${x.outPath}/src/**/*.purs"'')
            (builtins.attrValues spagoPkgs.inputs))}

        zephyr -f Main.main

        mkdir -p ./pf_app/css

        cp -r $src/pf_app/ ./

        NODE_ENV=production node node_modules/.bin/tailwindcss \
          build pf_app/css/tailwind.css -o pf_app/css/style.css -c $src/tailwind.config.js

        mkdir -p $out/dist

        node node_modules/.bin/parcel \
          build pf_app/prod/index.html --out-dir $out/dist/ --no-source-maps
      '';
    };
in
  nixpkgs.stdenv.mkDerivation {
    name = "site-frontend";
    src = ./.;
    installPhase = ''
      mkdir $out
      cp -r ${site-frontend-build}/dist $out
    '';
  }
