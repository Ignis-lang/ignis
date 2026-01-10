CC ?= gcc
CFLAGS ?= -O2 -g -fPIC -I.

MODULES = io string number types

.PHONY: all clean

all: $(foreach m,$(MODULES),$(m)/libignis_$(m).o)

io/libignis_io.o: io/io.c io/io.h
	$(CC) $(CFLAGS) -c io/io.c -o io/libignis_io.o

string/libignis_string.o: string/string.c string/string.h
	$(CC) $(CFLAGS) -c string/string.c -o string/libignis_string.o

number/libignis_number.o: number/number.c number/number.h
	$(CC) $(CFLAGS) -c number/number.c -o number/libignis_number.o

types/libignis_types.o: types/types.c types/types.h
	$(CC) $(CFLAGS) -c types/types.c -o types/libignis_types.o

clean:
	rm -f */libignis_*.o
