#include "App.hpp"
#include <cstdlib>
#include <iostream>
#include <sstream>

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "Usage: ./main <input.code> [extra gcc flags]\n";
    return 1;
  }

  try {
    App app(argv[1]);
    app.compile("out.c");

    // Build the GCC compile command
    std::stringstream compile_cmd;
    compile_cmd
        << "gcc -std=c99 -Wall -Wextra -Wpedantic "
           "-Wconversion -Wsign-conversion -Wshadow -Wfloat-equal -Wundef "
           "-Wcast-align -Wold-style-definition -Wformat=2 -Wformat-security "
           "-Wlogical-op -Wimplicit-fallthrough=5 -Wduplicated-cond "
           "-Wduplicated-branches "
           "-Wvla -Walloc-zero -Wnull-dereference -Wuninitialized "
           "-fanalyzer -fstrict-aliasing -Wstrict-aliasing=3 "
           "-fstack-protector-strong -D_FORTIFY_SOURCE=2 -fPIE -pie "
           "-O2 -fsanitize=undefined,address -lm out.c ";

    // Append extra user flags (like -lm, -lwhatever)
    for (int i = 2; i < argc; ++i) {
      compile_cmd << " " << argv[i];
    }

    // Compile and clean up

    std::cout << compile_cmd.str().c_str() << "\n";
    system(compile_cmd.str().c_str());
    system("rm out.c");

  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return 2;
  }

  return 0;
}
