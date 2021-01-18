# purefunctor.me
My personal portfolio website written in PureScript and Haskell.

## Development Requirements
The project requires the following compilers and build tools to be installed for development:

### Frontend
* purescript (v0.13.8)
* spago
* npm
* zephyr

### Backend
* ghc (8.8.4)
* cabal
* stack

### Optional
* nix

### Meta
* cabal2nix
* haskell-language-server

## Hacking the Project
Once you have these development dependencies installed, you can now start hacking on the project.

### Frontend
To start, install dependencies from `npm`:
```sh
λ npm install
```

To run a development server:
```sh
λ npm run dev
```

To create a production build:
```sh
λ npm run prod
```
The output is then stored in a directory named `static`.

### Backend
The Haskell backend can be built through multiple different ways, name through `cabal`, `stack`, and `nix`.

#### Cabal-based Builds
To start, ensure that you have GHC 8.8.4
```sh
λ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4
```

After which you can then build the packages using `cabal`:
```sh
λ cabal update  # Skip if already up-to-date
λ cabal build
```

#### Stack-based Builds
`stack` can download the appropriate GHC version for you depending on the resolver provided by the project itself if you haven't told `stack` to use the system GHC and/or you don't have a system-level GHC 8.8.4 installation.

To build the project:
```sh
λ stack update  # Skip if already up-to-date
λ stack build
```

#### Nix-powered Cabal Builds
The project also provides `nix` expressions for easier builds and dependency caching.
* [config.nix](./config.nix) - Determines package overrides and general configuration options for the project itself.
* [release.nix](./release.nix) - Determines how the package is built by `nix-build` using `cabal`.
* [project.nix](./project.nix) - Generated through the `cabal2nix` tool every time a `cabal` dependency is updated.
* [shell.nix](./shell.nix) - Provides a development shell that tricks `cabal` and other tools like `haskell-language-server` (when invoked inside of `nix-shell`) to use Nix packages when building the project.

To build the project:
```sh
λ nix-build release.nix
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

Currently, there are no plans of making Nix-assisted `stack` builds.

### Meta
The following section is concerned with the development of the project itself as a whole, detailing common project practices and development setups.

#### Haskell Language Server (HLS) Integration
`haskell-language-server` can be used as-is on both `cabal` and `stack` builds.  First and foremost, you have to tell `haskell-language-server` which build tool to use, after which you can launch your favorite text-editor with LSP integration like normal.

##### To use Cabal
```sh
λ ln -s hie-cabal.yaml hie.yaml
```

##### To use Stack
```sh
λ ls -s hie-stack.yaml hie.yaml
```

##### To use Nix-powered Cabal
`haskell-language-server` can also be assisted by Nix with project dependencies through `cabal`. To start, make sure that you're using the appropriate cradle:
```sh
λ ln -s hie-cabal.yaml hie.yaml
```
You can then enter `nix-shell` and invoke `haskell-language-server-wrapper` or any editor that uses `haskell-language-server` as long as the updated environment variables are taken into account:
```sh
your-user:λ nix-shell
...
nix-shell:λ haskell-language-server-wrapper
```

#### Dependency Management
Like most `cabal` or `stack`-based Haskell packages, the application makes use of a `*.cabal` file to manage dependencies, declare modules, and overall project configuration. `stack` and `nix` on the other hand makes sure that the project uses a consistent set of packages for reproducible builds.

Haskell dependencies must first be added to the [purefunctor-me.cabal](./purefunctor-me.cabal) file, after which `cabal2nix` has to be invoked:
```sh
λ cabal2nix . > project.nix
```

This step ensures that vanilla builds through `cabal` and `stack` stays in sync with Nix-powered `cabal` builds.

#### Nix Garbage Collection
If Nix ever has the need to compile Haskell dependencies for the project, it's advised, especially for lower-end machines, to have this package installed in order to make sure that the built dependencies don't get garbage collected:
```sh
λ nix-env -if release.nix
```
