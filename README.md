# purefunctor.me
My personal portfolio website written in PureScript and Haskell.

## Development Requirements
The project requires the following compilers and build tools to be installed for development:

### Frontend
* purescript (v0.13.8)
* spago
* yarn
* zephyr

### Backend
* ghc (8.8.4)
* cabal

### Optional
* nix
* cachix

### Meta
* haskell-language-server

## Hacking the Project
Once you have these development dependencies installed, you can now start hacking on the project. Alternatively, a `shell.nix` file is also provided for use with Nix and it contains the minimal tools needed for working on the project.

### Frontend
Install dependencies through `yarn` and check `package.json` for possible build scripts.

### Backend
The Haskell backend can be built either through `cabal` or `nix`.

#### Cabal-based Builds
To start, ensure that you have GHC 8.8.4
```sh
λ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4
```

After which you can then build the packages using `cabal`:
```sh
λ cabal build purefunctor-me
```

Likewise, a `hie.yaml` file is provided for use with `haskell-language-server`.

#### Nix-powered Cabal Builds
To build the project:
```sh
λ nix-build release-backend.nix
```

This should produce the `purefunctor-me` binary in a directory named `result`:
```sh
λ ./result/bin/purefunctor-me
```

Alternatively, you can go into `nix-shell` and use `cabal` as normal:
```sh
your-user:λ nix-shell
...
nix-shell:λ cabal build
```

One can also use `nix-shell` to invoke `haskell-language-server` for development. 

### Cachix Cache
This project uses the `applicative-labs` cache for its dependencies; to use the cache for development:
```sh
λ cachix use applicative-labs
```

## Deployment
The project uses `docker` and `docker-compose` for deployment; make sure you have both installed.

1) Create a `config.toml` in the `app` directory; a `config-default.toml` file is provided.

2) Run `docker-compose up --build`, this should bind the machine's port 80 to NGINX.
