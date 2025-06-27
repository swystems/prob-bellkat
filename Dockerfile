FROM nixos/nix:2.20.5

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

COPY flake.nix /opt/pbkat/flake.nix
COPY flake.lock /opt/pbkat/flake.lock
COPY package.yaml /opt/pbkat/package.yaml

WORKDIR /opt/pbkat

RUN nix develop

RUN nix develop --command cabal user-config init

ENV LANG=C.UTF-8

RUN echo "[safe] \
    directory = /opt/pbkat" > ~/.gitconfig

ENTRYPOINT ["nix", "develop"]
