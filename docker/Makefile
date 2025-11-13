all:

build:
	docker build -t ssr .

start:
	docker run -p 8080:8080 --name=ssr ssr

stop:
	docker stop ssr
	docker rm ssr

restart:
	-$(MAKE) stop
	$(MAKE) start

-include Makefile.local
