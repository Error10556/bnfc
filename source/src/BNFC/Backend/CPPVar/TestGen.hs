{-# LANGUAGE QuasiQuotes #-}

module BNFC.Backend.CPPVar.TestGen (testFilename, makeTest) where

import BNFC.Backend.CPPVar.CPPUtil
import Text.PrettyPrint
import Data.String.QQ

testFilename :: String
testFilename = "Test.cpp"

makeTest :: Doc
makeTest = linesToText $ lines [s|
#include <cstring>
#include <vector>

#include "Absyn.hpp"
//#include "PrettyPrinter.hpp"
#include "SyntaxPrinter.hpp"
#include "grammar.tab.hpp"
#include "PatternMatching.hpp"
using namespace std;

int main(int argc, char** argv) {
    if (!argc) {
        cerr << "0 arguments provided" << endl;
        return 1;
    }
    vector<char*> files;
    bool help = false, pretty = false, tree = false, onlyFiles = false;
    for (int i = 1; i < argc; i++) {
        char* arg = argv[i];
        if (arg[0] == 0) continue;
        if (onlyFiles) {
            files.push_back(arg);
            continue;
        }
        if (arg[0] != '-') {
            files.push_back(arg);
            continue;
        }
        if (arg[1] == 0) {
            files.push_back(arg);
            continue;
        }
        if (arg[1] == '-') {
            if (arg[2] == 0)
                onlyFiles = true;
            else if (strcmp(arg + 2, "help") == 0)
                help = true;
            else if (strcmp(arg + 2, "pretty") == 0)
                pretty = true;
            else if (strcmp(arg + 2, "tree") == 0)
                tree = true;
            else {
                cerr << "Invalid option: " << arg << endl;
                return 1;
            }
            continue;
        }
        for (char* i = arg + 1; *i; ++i) {
            switch (*i) {
                case 'h':
                    help = true;
                    break;
                case 'p':
                    pretty = true;
                    break;
                case 't':
                    tree = true;
                    break;
                default:
                    cerr << "Invalid option: -" << *i << endl;
                    return 1;
            }
        }
    }
    if (!help && files.empty()) {
        cerr << "Nothing to do. Run `" << argv[0] << " --help' for help"
             << endl;
        return 0;
    }
    if (help) {
        cerr << "Sample syntax parser.\\nUsage: \\n"
             << argv[0] << " (OPTION|FILE)... [-- FILE...]\\n";
        cerr << R"%(
Options:
  -h --help    Display this message
  -p --pretty  Pretty-print the abstract syntax tree
  -t --tree    Print the abstract syntax tree like a tree
     --        Treat the remaining arguments as files
)%";
    }
    for (char* filename : files) {
        FILE* file; bool needclose;
        if (strcmp(filename, "-") == 0) {
            file = stdin;
            needclose = false;
        }
        else {
            file = fopen(filename, "r");
            if (!file) {
                cerr << "Cannot open file ";
                perror(filename);
                return 1;
            }
            needclose = true;
        }
        cout << filename << '\\n';

        LC::ParseProgram(file) | PatternMatch{
            [&](LC::Parser::syntax_error&& err) {
                cout << "Could not parse!\\nError: " << err.what() << "\\n\\n";
                return;
            },
            [&](LC::Program&& p) {
                if (tree) {
                    p | LC::SyntaxPrinter(cout);
                    cout << '\\n';
                }
                if (pretty) {
                    //p | LC::PrettyPrinter(cout);
                    cout << "PrettyPrinting is not implemented";
                    cout << "\\n\\n";
                }
                if (!tree && !pretty) cout << "OK\\n\\n";
            }
        };

        if (needclose) fclose(file);
    }
}
|]
