CC = gcc

default	: cbsl.c
	${CC} -O0 -ggdb -o cbsl-kernel cbsl.c

clean :
	rm -f cbsl-kernel
