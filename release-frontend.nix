let
  config = import ./config.nix { };
  inherit (config) nixpkgs pursPkgs;

  spagoPkgs = import ./spago-packages.nix { pkgs = nixpkgs; };

  site-frontend-build =
    nixpkgs.mkYarnPackage rec {
      name = "site-frontend-build";
      src = ./.;
      packageJSON = ./package.json;
      yarnLock = ./yarn.lock;

      nativeBuildInputs = [
        pursPkgs.purs
        pursPkgs.zephyr
        nixpkgs.nodejs-15_x
      ];

      postBuild = ''
        ${pursPkgs.purs}/bin/purs compile --codegen corefn "$src/pf_app/**/*.purs" ${builtins.toString
          (builtins.map
            (x: ''"${x.outPath}/src/**/*.purs"'')
            (builtins.attrValues spagoPkgs.inputs))}

        zephyr -f Main.main

        mkdir -p ./pf_app/css

        cp -r $src/pf_app/ ./

        cp $src/tailwind.config.js ./

        NODE_ENV=production ${nixpkgs.nodejs-15_x}/bin/node node_modules/.bin/tailwindcss \
          build pf_app/css/tailwind.css -o pf_app/css/style.css

        mkdir -p $out/dist

        ${nixpkgs.nodejs-15_x}/bin/node node_modules/.bin/parcel \
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
