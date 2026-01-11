CC ?= gcc
CFLAGS ?= -O2 -g -fPIC -I.

.PHONY: all clean

# Core runtime must be built first, all other modules depend on it
all: libignis_rt.o io/libignis_io.o string/libignis_string.o number/libignis_number.o types/libignis_types.o memory/libignis_memory.o

# Core runtime
libignis_rt.o: ignis_rt.c ignis_rt.h
	$(CC) $(CFLAGS) -c ignis_rt.c -o libignis_rt.o

# Modules depend on core runtime
io/libignis_io.o: io/io.c io/io.h ignis_rt.h libignis_rt.o
	$(CC) $(CFLAGS) -c io/io.c -o io/libignis_io.o

string/libignis_string.o: string/string.c string/string.h ignis_rt.h libignis_rt.o
	$(CC) $(CFLAGS) -c string/string.c -o string/libignis_string.o

number/libignis_number.o: number/number.c number/number.h ignis_rt.h libignis_rt.o
	$(CC) $(CFLAGS) -c number/number.c -o number/libignis_number.o

types/libignis_types.o: types/types.c types/types.h ignis_rt.h libignis_rt.o
	$(CC) $(CFLAGS) -c types/types.c -o types/libignis_types.o

memory/libignis_memory.o: memory/memory.c memory/memory.h ignis_rt.h libignis_rt.o
	$(CC) $(CFLAGS) -c memory/memory.c -o memory/libignis_memory.o

clean:
	rm -f libignis_rt.o
	rm -f io/libignis_io.o
	rm -f string/libignis_string.o
	rm -f number/libignis_number.o
	rm -f types/libignis_types.o
	rm -f memory/libignis_memory.o
