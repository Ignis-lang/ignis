CC ?= gcc
AR ?= ar
CFLAGS ?= -O2 -g -fPIC -I. -Wall -Wextra

INTERNAL_SRCS = internal/rt_memory.c internal/rt_string.c internal/rt_rc.c internal/rt_fs.c
INTERNAL_OBJS = $(INTERNAL_SRCS:.c=.o)

.PHONY: all clean test test-san

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
	rm -f tests/test_smoke

# Smoke tests
test: libignis_rt.a
	@mkdir -p tests
	@if [ -f tests/test_smoke.c ]; then \
		$(CC) $(CFLAGS) tests/test_smoke.c -L. -lignis_rt -o tests/test_smoke && \
		./tests/test_smoke && \
		echo "Tests passed"; \
	else \
		echo "No tests found (tests/test_smoke.c)"; \
	fi

# Smoke tests with leak sanitizer
test-san: libignis_rt.a
	@mkdir -p tests
	@if [ -f tests/test_smoke.c ]; then \
		$(CC) $(CFLAGS) -fsanitize=leak tests/test_smoke.c -L. -lignis_rt -o tests/test_smoke && \
		./tests/test_smoke && \
		echo "Tests passed (with leak sanitizer)"; \
	else \
		echo "No tests found (tests/test_smoke.c)"; \
	fi
