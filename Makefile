MSG = "debug"
SRC = epidemic.tex
TARGET = $(SRC:%.tex=%.pdf)

all:	$(TARGET)

$(TARGET):	$(SRC)
	latexmk $(SRC)

push:
	git add -u
	git commit -m ${MSG}
	git push -u origin master

clean:
	latexmk -C $(SRC)
	rm -f *~

