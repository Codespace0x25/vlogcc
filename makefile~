# CodeVoid basic (CPP) Makefile

# Tools
ASM     = nasm
CC      = clang++

# Project Structure
SRC     = src
BUILD   = build
TARGET  = $(BUILD)/main

# Flags
CFLAGS  =  -Wall -Wextra -O2 
LDFLAGS =  -pthread -lm

# Sources
CXX_SOURCES   := $(shell find $(SRC) -name '*.cpp')

# Objects
CXX_OBJS      := $(patsubst $(SRC)/%.c,$(BUILD)/%.o,$(CXX_SOURCES))
OBJS        := $(CXX_OBJS)

.PHONY: all clean run

# Default target
all: $(TARGET)

# Compile C files
$(BUILD)/%.o: $(SRC)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@


# Link all object files
$(TARGET): $(OBJS)
	@mkdir -p $(dir $@)
	$(CC) -o $@ $^ $(LDFLAGS)

# Run the output binary
run: $(TARGET)
	./$(TARGET)

# Clean build artifacts
clean:
	rm -rf $(BUILD)
