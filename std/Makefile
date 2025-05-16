.PHONY: all clean

CC = gcc
CFLAGS = -Wall -Wextra -std=c11
SRC_DIRS := $(shell find . -type f -name '*.c' -exec dirname {} \; | sort -u)
C_FILES := $(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.c))
OBJ_FILES := $(C_FILES:.c=.o)

all: $(OBJ_FILES)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	find . -type f -name '*.o' -delete

