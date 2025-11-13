all:

build:
	docker build --no-cache -t ssr .

start:
	docker run -p 8080:8080 --name=ssr ssr

stop:
	docker stop ssr
	docker rm ssr

restart:
	-$(MAKE) stop
	$(MAKE) start

-include Makefile.local
