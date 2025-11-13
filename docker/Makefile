ADMIN = $(shell cat admin)

all:

build:
	docker build -t ssr --build-arg ADMIN=$(ADMIN) .

start:
	docker run -p 8080:8080 --name=ssr ssr

stop:
	docker stop ssr
	docker rm ssr

restart:
	-$(MAKE) stop
	$(MAKE) start

-include Makefile.local
