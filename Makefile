elm:
	cd ui/ && elm-make Test.elm --output tests.html && mv tests.html ../server/

build: elm
	cd server/ && stack build

start:
	cd server/ && stack exec aafa-server

go: clean elm start

clean:
	rm -f server/aafa.db
