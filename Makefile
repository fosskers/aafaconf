elm:
	cd ui/  && elm-make Day1.elm --output register.html \
		&& elm-make Day2.elm --output groups.html \
		&& mv *.html ../assets/

build: elm
	cd server/ && stack build

start:
	cd server/ && stack exec aafa-server

go: clean elm start

clean:
	rm -f server/aafa.db
