FROM debian:testing-slim

RUN apt-get -y update

RUN apt-get -y install build-essential ocaml opam

WORKDIR /home

RUN opam init -y --bare --disable-sandboxing
RUN opam switch create system --empty
RUN apt-get -y install libev-dev libgmp-dev libssl-dev pkg-config
RUN opam install -y dune dream
RUN opam clean

RUN git clone https://github.com/smimram/student-screen-recorder.git ssr

WORKDIR /home/ssr

CMD eval $(opam env) && git pull && make && dune exec -- src/server.exe --admin-password ${ADMIN}
