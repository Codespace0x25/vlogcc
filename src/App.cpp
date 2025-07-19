#include <iostream>
#include "App.hpp"

namespace App {
  using json = nlohmann::json;

  App::App(const std::string& name, const std::string& version)
    : appname(name), appVersion(version) {
    std::cout << "App: " << name << " v" << version << "\n";

    // Initialize function environment with some basic functions
    funcEnv = {
      {"+", [](const json& args) {
        int sum = 0;
        for (const auto& v : args) sum += v.get<int>();
        return sum;
      }},
      {"-", [](const json& args) {
        if (args.size() == 1) return -args[0].get<int>();
        int res = args[0].get<int>();
        for (size_t i = 1; i < args.size(); ++i)
          res -= args[i].get<int>();
        return res;
      }},
      {"*", [](const json& args) {
        int prod = 1;
        for (const auto& v : args) prod *= v.get<int>();
        return prod;
      }},
      {"/", [](const json& args) {
        int res = args[0].get<int>();
        for (size_t i = 1; i < args.size(); ++i)
          res /= args[i].get<int>();
        return res;
      }},
      {">", [](const json& args) {
        return args[0].get<int>() > args[1].get<int>();
      }},
      {"print", [](const json& args) {
        for (const auto& v : args) std::cout << v << " ";
        std::cout << "\n";
        return nullptr;
      }}
    };
  }

  App::~App() {}

  json App::eval(const json& expr) {
    // Variable or symbol
    if (expr.is_string()) {
      std::string var = expr.get<std::string>();
      if (env.find(var) != env.end())
        return env[var];
      return expr; // symbol itself if not found
    }

    // Literal (number, bool, null)
    if (expr.is_number() || expr.is_boolean() || expr.is_null()) {
      return expr;
    }

    // Function call (list)
    if (expr.is_array()) {
      if (expr.empty()) return nullptr;
      std::string funcName = expr[0].get<std::string>();
      json args = json::array();
      for (size_t i = 1; i < expr.size(); ++i) {
        args.push_back(eval(expr[i]));
      }

      if (funcEnv.find(funcName) != funcEnv.end()) {
        return funcEnv[funcName](args);
      } else {
        std::cerr << "Unknown function: " << funcName << "\n";
        return nullptr;
      }
    }

    // Special forms (objects)
    if (expr.is_object()) {
      if (expr.contains("define")) {
        std::string var = expr["define"][0].get<std::string>();
        json val = eval(expr["define"][1]);
        env[var] = val;
        return nullptr;
      }
      if (expr.contains("if")) {
        json cond = eval(expr["if"]["cond"]);
        if (cond.is_boolean() && cond.get<bool>())
          return eval(expr["if"]["then"]);
        else
          return eval(expr["if"]["else"]);
      }

      // Treat other objects as data
      return expr;
    }

    return nullptr;
  }

  void App::run(const json& program) {
    for (const auto& expr : program) {
      eval(expr);
    }
  }
}
