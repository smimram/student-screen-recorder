FROM debian:stable-slim

RUN apt-get -y update

RUN apt-get -y install build-essential ocaml opam

WORKDIR /home

RUN opam init -y --bare --disable-sandboxing
RUN opam switch create system --empty
RUN apt-get -y install libev-dev libgmp-dev libssl-dev pkg-config
RUN opam install -y dune dream yaml
RUN opam clean

RUN git clone https://github.com/smimram/student-screen-recorder.git ssr

COPY ssr.yml /home/ssr/
COPY events.yml /home/ssr/

WORKDIR /home/ssr

CMD ["sh", "-c", "eval $(opam env) && git pull && make && dune exec -- src/ssr.exe 2>&1 | tee -a screenshots/ssr.log"]
