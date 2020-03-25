MSG = debug
SRC = epidemic
push:
	git add -u
	git commit -m ${MSG}
	git push -u origin master

clean:
	latexmk -C $(SRC)
	rm -f *~

