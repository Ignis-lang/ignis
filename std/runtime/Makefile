CC ?= gcc
AR ?= ar
CFLAGS ?= -O2 -g -fPIC -I. -Wall -Wextra

INTERNAL_SRCS = internal/rt_fs.c
INTERNAL_OBJS = $(INTERNAL_SRCS:.c=.o)

.PHONY: all clean

all: libignis_rt.a

# Pattern rule for internal objects
internal/%.o: internal/%.c internal/rt_internal.h ignis_rt.h
	$(CC) $(CFLAGS) -c $< -o $@

# Archive from internal objects
libignis_rt.a: $(INTERNAL_OBJS)
	rm -f $@
	$(AR) rcs $@ $(INTERNAL_OBJS)

clean:
	rm -f libignis_rt.a
	rm -f internal/*.o
