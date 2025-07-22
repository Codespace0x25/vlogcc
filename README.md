# Vlog Compiler (`vlogcc`)

A minimalist compiler for the **VLOG** language — a system-level language with its own syntax that compiles down to optimized native binaries via C code and `gcc` behind the scenes. Includes a small standard library and support for modular `require`s.

---

## Features

* Supports:

  * `defun`, `type`, `macro`, `extern`, `mut`, `require`
* Automatically compiles the final C output into an executable using `gcc`
* Accepts standard `gcc` flags after your input file
* Standard library included in `lib/`
* Simple compiler that can bootstrap itself from a `.code` file
* Requires only `g++` and `make` to build

---

## Build

To compile the `vlogcc` compiler:

```
make
```

To clean build files:

```
make clean
```

To test (runs tests in `tests/`):

```
make test
```

---

## Usage

To compile and run a `.code` file:

```
vlogcc path/to/yourfile.code -o yourprogram
```

Or pass any other `gcc`-style arguments:

```
vlogcc path/to/main.code -Wall -O2 -o hello
./hello
```

This will:

1. Preprocess `main.code` and all `require`s
2. Convert to temporary C code internally
3. Compile it directly to a binary using `gcc`
4. Output the final binary as specified (default is `a.out` if `-o` is omitted)

---

## Example

```
require "lib/io.code";

defun main(void) int {
    mut int x = 42;
    printf("Hello! x = %d\n", x);
    return 0;
}
```

Compile and run:

```
vlogcc myapp.code -o myapp
./myapp
```

---

## require behavior

When using `require "file.code"`:

* First checks relative to the current `.code` file
* Then tries the environment variable `VLOG_LIB`
* Then finally falls back to `/usr/include/vlog`

Set your lib path like this:

```
export VLOG_LIB=/usr/include/vlog
```

---

## Installation

To install the compiler and its standard library:

```
sudo make install
```

This will install:

* `vlogcc` binary to `/bin/` (or your `PREFIX`)
* All files in `lib/` to `/usr/include/vlog/` (or your `PREFIX_lib`)

You can customize install locations:

```
make install PREFIX=/usr PREFIX_lib=/usr/local
```

---

## License

This project is released **with no license**.
Use it however you want — no credit needed, no restrictions.
You're free to fork, modify, redistribute, or build upon it.

---

## Author

Made by Olivia — a queer system hacker who codes for love and expression.
Passionate about minimalism, custom languages, purple things, and writing real compilers from scratch.

