#pragma once
#include <string>
#include <unordered_map>
#include <functional>
#include "json.hpp"

namespace App {
  using json = nlohmann::json;
  using Func = std::function<json(const json&)>;

  class App {
  public:
    App(const std::string& name, const std::string& version);
    ~App();

    void run(const json& program);

  private:
    std::string appname;
    std::string appVersion;

    // Separate environments
    std::unordered_map<std::string, json> env;       // variables
    std::unordered_map<std::string, Func> funcEnv;   // functions

    json eval(const json& expr);
  };
}
