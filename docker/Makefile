ADMIN = $(shell cat admin)

all:

build: ssr.yml events.yml
	docker build -t ssr .

rebuild: ssr.yml events.yml
	docker build -t ssr . --no-cache

start:
	docker run -p 8080:8080 --name=ssr ssr

stop:
	docker stop ssr
	docker rm ssr

restart:
	-$(MAKE) stop
	$(MAKE) start

ssr.yml events.yml:
	touch $@

-include Makefile.local
