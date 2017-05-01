STACK ?= /bin/stack
PORT ?= 8081

elm:
	cd ui/  && elm-make Day1.elm --output register.html \
		&& elm-make Day2.elm --output groups.html \
		&& elm-make Day3.elm --output signin.html \
		&& mv *.html ../assets/

build: elm
	cd server/ && $(STACK) build

start:
	cd server/ && $(STACK) exec -- aafa-server --port $(PORT)

go: build start

clean:
	rm -f server/aafa.db
