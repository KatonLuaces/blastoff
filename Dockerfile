# Based on 20.04 LTS
FROM ubuntu:focal

RUN DEBIAN_FRONTEND=noninteractive apt-get -yq update && \
    DEBIAN_FRONTEND=noninteractive apt-get -y upgrade && \
    DEBIAN_FRONTEND=noninteractive apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    menhir \
    llvm-10 \
    llvm-10-dev \
    m4 \
    git \
    aspcud \
    ca-certificates \
    python2.7 \
    pkg-config \
    cmake \
    opam \
    clang-10

RUN ln -s /usr/bin/lli-10 /usr/bin/lli
RUN ln -s /usr/bin/llc-10 /usr/bin/llc
RUN ln -s /usr/bin/clang-10 /usr/bin/clang

RUN opam init --disable-sandboxing
RUN opam install -y \
    llvm.10.0.0 \
    ocamlfind \
    ocaml-lsp-server \
    merlin \
    ocamlformat

COPY GraphBLAS.h /usr/local/include/GraphBLAS.h
COPY libgraphblas.so.4.0.3 /usr/local/lib/libgraphblas.so.4.0.3
RUN ln -s /usr/local/lib/libgraphblas.so.4 /usr/local/lib/libgraphblas.so
RUN ln -s /usr/local/lib/libgraphblas.so.4.0.3 /usr/local/lib/libgraphblas.so.4
ENV LD_LIBRARY_PATH=/usr/local/lib

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"](base)