CC := gcc
CFLAGS := -Wall -Wpedantic -std=c99

SOURCES := main.c
OBJECTS := $(SOURCES:%.c=%.o)
TARGETS := example

CLEAN_FILES := $(TARGETS) $(OBJECTS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

$(TARGETS): $(OBJECTS)
	$(CC) -o $@ $^ -lm

clean:
	rm -rf $(CLEAN_FILES)

install: $(TARGETS)
	@echo "Nothing to install"
