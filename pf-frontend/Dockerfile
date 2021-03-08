FROM nixos/nix as build

WORKDIR /src

COPY . .

RUN nix-shell --arg forBuild true --arg forHsDev false --run "yarn"\
 && nix-shell --arg forBuild true --arg forHsDev false --run "yarn run prod:zephyr"

FROM scratch

COPY --from=build /src/pf-frontend/dist /dist
