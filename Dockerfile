FROM haskell:9-slim as builder
WORKDIR /tmp/parthenon
RUN cabal update

COPY parthenon.cabal /tmp/parthenon/
RUN cabal build --only-dependencies -j4

COPY . /tmp/parthenon/
RUN cabal install

FROM scratch
LABEL maintainer="Antoine Gagné <gagnantoine@gmail.com>"
COPY --from=builder /root/.cabal/bin/parthenon /usr/bin/
ENTRYPOINT ["parthenon"]
