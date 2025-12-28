FROM haskell:9.6.7-slim-bullseye AS builder

WORKDIR /app

COPY cabal.project ./
COPY cabal.project.freeze ./
COPY hsimev.cabal ./

RUN cabal update

RUN apt-get update && apt-get install -y libgmp-dev pkgconf zlib1g-dev && apt-get clean

COPY . .

RUN cabal build


FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp-dev pkgconf zlib1g-dev && apt-get clean

COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-9.6.7/hsimev-0.1.0.0/x/hsimev/build/hsimev/hsimev /usr/local/bin/hsimev

ENTRYPOINT ["hsimev"]
EXPOSE 3000
