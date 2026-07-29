FROM nixos/nix:2.20.5

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

ENV LANG=C.UTF-8
ENV MPLBACKEND=Agg

WORKDIR /opt/pbkat

COPY flake.nix flake.lock package.yaml ./

# Download and cache the complete development environment in the image.
RUN nix develop --command true
RUN nix develop --command cabal user-config init

RUN echo "[safe] \
    directory = /opt/pbkat" > /root/.gitconfig

ENTRYPOINT ["nix", "develop", "--command"]
CMD ["bash"]
