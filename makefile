# CodeVoid basic (CPP) Makefile

# --- Tools ---
ASM     = nasm
CC      = g++

# --- Project Structure ---
SRC     = src
BUILD   = build
TEST    = tests
TARGET  = $(BUILD)/main

# --- Flags ---
CFLAGS  = -std=c++17 -Wall -Wextra -O2
LDFLAGS = -pthread -lm

# --- Sources ---
CXX_SOURCES := $(shell find $(SRC) -name '*.cpp')
CXX_OBJS    := $(patsubst $(SRC)/%.cpp, $(BUILD)/%.o, $(CXX_SOURCES))
OBJS        := $(CXX_OBJS)

# --- Targets ---
.PHONY: all clean run test install debug

# Default target
all: $(TARGET)

# Compile C++ source files
$(BUILD)/%.o: $(SRC)/%.cpp
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

# Link object files into final binary
$(TARGET): $(OBJS)
	@mkdir -p $(dir $@)
	$(CC) -o $@ $^ $(LDFLAGS)

# Run the output binary
run: $(TARGET)
	./$(TARGET)

# Debug build (adds -g)
debug:
	$(MAKE) CFLAGS="-std=c++17 -Wall -Wextra -g" all

# Install binary and stdlib
PREFIX ?= /
PREFIX_lib ?= /usr
install: $(TARGET)
	@echo "Installing binary to $(PREFIX)/bin/"
	@sudo mkdir -p "$(PREFIX)/bin"
	@sudo cp "$(TARGET)" "$(PREFIX)/bin/vlogcc"
	@echo "Installing stdlib to $(PREFIX_lib)/include/vlog/"
	@if [ -d lib ]; then \
		sudo mkdir -p "$(PREFIX_lib)/include/vlog/"; \
		sudo cp -r lib/* "$(PREFIX_lib)/include/vlog/"; \
	else \
		echo "Warning: lib/ directory not found. Skipping lib install."; \
	fi

# Run tests (from ./tests directory)
test:
	@$(MAKE) -C $(TEST)

# Clean all build files
clean:
	rm -rf $(BUILD)
