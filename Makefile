MSG = debug
SRC = epidemic
push:
	git add --all
	git commit -m ${MSG}
	git push -u origin master

clean:
	latexmk -C $(SRC)
	rm -f *~

