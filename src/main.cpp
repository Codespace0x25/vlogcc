#include <iostream>
#include <fstream>
#include "App.hpp"
#include "json.hpp"

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <program.json>\n";
    return 1;
  }

  std::ifstream file(argv[1]);
  if (!file) {
    std::cerr << "Failed to open file: " << argv[1] << "\n";
    return 1;
  }

  nlohmann::json program;
  try {
    file >> program;
  } catch (const std::exception& e) {
    std::cerr << "JSON parse error: " << e.what() << "\n";
    return 1;
  }

  App::App app("JSONLang", "0.1");
  app.run(program);

  return 0;
}
