# Based on 20.04 LTS
FROM ubuntu:focal

RUN apt-get -yq update && \
    apt-get -y upgrade && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    cmake \
    llvm \
    llvm-dev \
    llvm-runtime \ 
    m4 \
    opam

RUN ln -s /usr/bin/lli-10.0 /usr/bin/lli
RUN ln -s /usr/bin/llc-10.0 /usr/bin/llc

RUN opam init
RUN opam install \
    llvm.10.0.0 \
    dune

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]