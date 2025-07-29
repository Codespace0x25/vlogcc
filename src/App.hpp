#pragma once
#include <string>

class App {
public:
  App(const std::string &path);
  void compile(const std::string &outputPath);

private:
  std::string inputPath;
  std::string inputCode;
  std::string currentDir;

  std::string parse();
  std::string preprocess(const std::string &code, const std::string &basePath);
  std::string trim(const std::string &str);
  std::string indent(const std::string &code, int level = 1);
};
