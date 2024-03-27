
allbutDriver = ast.go backend.go import.go parser.go predef.go rewrite.go scope.go symbolic.go typecheck.go typecheckx.go token.go types.go utils.go

d8m:	cdriver.go $(allbutDriver)
	go build -o d8mc cdriver.go $(allbutDriver)

runtests: testdriver.go $(allbutDriver)
	go build -o runtests testdriver.go $(allbutDriver)

install: d8m
	mv d8mc /usr/local/bin
	mkdir ${HOME}/Library/Application\ Support/D8m
	cp -p -r modules ${HOME}/Library/Application\ Support/D8m
	echo "[]" > ${HOME}/Library/Application\ Support/D8m/modules/modulesDB.json
	cd tests; `for f in *.d8m ; do touch "$${f%.d8m}.go" ; done`
