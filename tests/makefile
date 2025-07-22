#csode
MYDIR := .
BIN := $(MYDIR)/bin/
CC = ../build/main

.PHONY: default build clean

default: build

build: clean
	@mkdir -p $(BIN)
	@success=0; fail=0; \
	for file in $(MYDIR)/*.code; do \
		name=$(basename $$file .code); \
		out="$(BIN)$$name.out"; \
		echo "Compiling $$file..."; \
		if $(CC) $$file -o $$out $(shell pkg-config --cflags raylib) $(shell pkg-config --libs raylib); then \
			printf "%s -> %s [\033[0;32mS\033[0m]\n" $$file $$out; \
			success=$$(($$success+1)); \
		else \
			printf "%s -> %s [\033[0;31mF\033[0m]\n" $$file $$out; \
			fail=$$(($$fail+1)); \
		fi; \
	done; \
	echo ""; \
	echo "##################################"; \
	echo "##        Build Summary:        ##"; \
	echo "##        Success: $$success        ##"; \
	echo "##        Failed : $$fail        ##"; \
	echo "##################################"; \
	echo "##################################"; \
	[ $$fail -eq 0 ]
	rm -rf out.c

clean:
	@rm -rf $(BIN)

