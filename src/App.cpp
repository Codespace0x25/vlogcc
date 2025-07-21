#include "App.hpp"
#include <fstream>
#include <sstream>
#include <iostream>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;

App::App(const std::string& path) : inputPath(path) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("Could not open file: " + path);
    std::ostringstream ss;
    ss << in.rdbuf();
    inputCode = ss.str();

    currentDir = fs::absolute(fs::path(path)).parent_path();
}

void App::compile(const std::string& outputPath) {
    std::ofstream out(outputPath);
    if (!out) throw std::runtime_error("Failed to open output file");

    std::string parsedCode = parse();
    out << parsedCode;

    std::cout << "Compiled to " << outputPath << "\n";
}

std::string App::parse() {
    std::string preprocessed = preprocess(inputCode, currentDir);
    return preprocessed;
}

std::string App::preprocess(const std::string& code, const std::string& basePath) {
    std::istringstream in(code);
    std::ostringstream out;
    std::string line;

    std::regex requireRegex(R"regex(^\s*require\s+"(.+?)"\s*;?)regex");
    std::regex externRegex(R"(^\s*extern\s+(\w[\w\d\*]*)\s+(\w+)\s*\((.*?)\)\s*;?)");
    std::regex defunRegex(R"(^\s*defun\s+(\w+)\s*\((.*?)\)\s*(\w[\w\d\*]*)\s*\{)");

    // For mutability & const handling
    std::regex mutInitRegex(R"(^\s*mut\s+(\w[\w\d\*]*)\s+(\w+)\s*=\s*(.+)\s*;?)");
    std::regex mutDeclRegex(R"(^\s*mut\s+(\w[\w\d\*]*)\s+(\w+)\s*;?)");
    std::regex constInitRegex(R"(^\s*(\w[\w\d\*]*)\s+(\w+)\s*=\s*(.+)\s*;?)");
    std::regex constDeclRegex(R"(^\s*(\w[\w\d\*]*)\s+(\w+)\s*;?)");

    while (std::getline(in, line)) {
        line = trim(line);
        std::smatch match;

        if (std::regex_match(line, match, requireRegex)) {
            std::string reqPath = match[1].str();
            fs::path fullPath = fs::absolute(fs::path(basePath) / reqPath);
            std::ifstream reqFile(fullPath);
            if (!reqFile) throw std::runtime_error("Could not open required file: " + fullPath.string());
            std::ostringstream reqContent;
            reqContent << reqFile.rdbuf();
            out << "// --- require: " << reqPath << " ---\n";
            out << preprocess(reqContent.str(), fullPath.parent_path().string()) << "\n";
        }
        else if (std::regex_match(line, match, externRegex)) {
            std::string retType = match[1];
            std::string name = match[2];
            std::string args = match[3];
            out << retType << " " << name << "(" << args << ");\n";
        }
        else if (std::regex_match(line, match, defunRegex)) {
            std::string name = match[1];
            std::string args = match[2];
            std::string retType = match[3];
            out << retType << " " << name << "(" << args << ") {\n";

            std::string bodyLine;
            while (std::getline(in, bodyLine)) {
                std::string trimmed = trim(bodyLine);
                std::smatch bodyMatch;

                if (trimmed.empty()) {
                    out << "\n";
                }
                // return statement (catch first!)
                else if (trimmed.rfind("return ", 0) == 0 || trimmed == "return;" || trimmed.rfind("return(", 0) == 0) {
                    out << indent(trimmed) << "\n";
                }
                // mutable with init
                else if (std::regex_match(trimmed, bodyMatch, mutInitRegex)) {
                    out << indent(bodyMatch[1].str() + " " + bodyMatch[2].str() + " = " + bodyMatch[3].str() + ";") << "\n";
                }
                // mutable without init
                else if (std::regex_match(trimmed, bodyMatch, mutDeclRegex)) {
                    out << indent(bodyMatch[1].str() + " " + bodyMatch[2].str() + ";") << "\n";
                }
                // const with init
                else if (std::regex_match(trimmed, bodyMatch, constInitRegex)) {
                    out << indent("const " + bodyMatch[1].str() + " " + bodyMatch[2].str() + " = " + bodyMatch[3].str() + ";") << "\n";
                }
                // const without init
                else if (std::regex_match(trimmed, bodyMatch, constDeclRegex)) {
                    out << indent("const " + bodyMatch[1].str() + " " + bodyMatch[2].str() + ";") << "\n";
                }
                // closing brace ends block
                else if (trimmed == "}") {
                    break;
                }
                // fallback
                else {
                    out << indent(trimmed) << "\n";
                }
            }

            out << "}\n";
        }
        else {
            out << line << "\n";
        }
    }

    return out.str();
}

std::string App::trim(const std::string& str) {
    const char* ws = " \t\n\r";
    size_t start = str.find_first_not_of(ws);
    size_t end = str.find_last_not_of(ws);
    return (start == std::string::npos) ? "" : str.substr(start, end - start + 1);
}

std::string App::indent(const std::string& code, int level) {
    return std::string(level * 4, ' ') + trim(code);
}
