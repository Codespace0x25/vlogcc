# CodeVoid basic (CPP) Makefile
# Tools
ASM     = nasm
CC      = g++

# Project Structure
SRC     = src
BUILD   = build
TEST   = tests
TARGET  = $(BUILD)/main


# Flags
CFLAGS  = -std=c++17 -Wall -Wextra -O2
LDFLAGS = -pthread -lm

# Sources
CXX_SOURCES := $(shell find $(SRC) -name '*.cpp')
CXX_OBJS    := $(patsubst $(SRC)/%.cpp, $(BUILD)/%.o, $(CXX_SOURCES))
OBJS        := $(CXX_OBJS)

.PHONY: all clean run

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

# Run the tests from the root directory
test: 
	@$(MAKE) -C tests

# Clean build artifacts
clean:
	rm -rf $(BUILD)
