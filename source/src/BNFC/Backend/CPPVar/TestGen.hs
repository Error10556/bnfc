{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.TestGen
  Description : Generates an example parser for testing.

  Generates an example parser for testing.
-}

module BNFC.Backend.CPPVar.TestGen
  (
    -- * The entrypoint
    makeTest

    -- * File naming
  , testFilename
  ) where

import Data.String.QQ
import Text.PrettyPrint

import qualified BNFC.Options as Options
import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the source file.
testFilename :: String
testFilename = "Test.cpp"

-- | Generates the example parser code.
makeTest :: Options.SharedOptions -> Doc
makeTest opts = let
    ns = case Options.inPackage opts of
      Nothing   -> ""
      Just name -> name ++ "::"
  in unlinesToText [s|
#include <cstring>
#include <vector>

#include "HaskellPrinter.hpp"
#include "PrettyPrinter.hpp"
#include "SyntaxPrinter.hpp"
|]
  $+$ text ("#include \"" ++ Options.lang opts ++ ".tab.hpp\"")
  $+$ unlinesToText [s|
#include "PatternMatching.hpp"
using namespace std;

bool help = false, pretty = false, tree = false, haskell = false,
    systest = false;

int main(int argc, char** argv) {
    if (!argc) {
        cerr << "0 arguments provided" << endl;
        return 1;
    }
    bool onlyFiles = false;
    vector<const char*> files;
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
            else if (strcmp(arg + 2, "haskell") == 0)
                haskell = true;
            else if (strcmp(arg + 2, "systest") == 0)
                systest = true;
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
                case 'H':
                    haskell = true;
                    break;
                case 's':
                    systest = true;
                    break;
                default:
                    cerr << "Invalid option: -" << *i << endl;
                    return 1;
            }
        }
    }
    if (!help && files.empty())
        files.push_back("-");
    if (help) {
        cout << "Example syntax parser.\nUsage:\n"
             << argv[0] << " (OPTION|FILE)... [-- FILE...]\n";
        cout << R"%(
Options:
  -h --help     Display this message.
  -p --pretty   Pretty-print the abstract syntax tree.
  -t --tree     Print the abstract syntax tree like a tree.
  -H --haskell  Print the abstract syntax tree as a Haskell expression.
  -s --systest  For use in BNFC system tests. Overrides other options.
     --         Treat  the remaining arguments as files.

If no files are specified of if FILE is -, read standard input.

The exit code is the number of files for which parsing failed. In particular,
if every file is parsed successfully, the exit code will be 0.
)%";
    }
    bool printFilenames = files.size() > 1;
    int errorcount = 0;
    for (const char* filename : files) {
        FILE* file; bool needclose;
        if (strcmp(filename, "-") == 0) {
            file = stdin;
            needclose = false;
            cerr << "Reading from stdin..." << endl;
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
        if (printFilenames) cerr << filename << endl;

|]
  $+$ linesToText
  [ "        " ++ ns ++ "Parse(file) | PatternMatch{"
  , "            [&](" ++ ns ++ "Parser::syntax_error&& err) {"
  , "                cerr << \"Could not parse!\\nError: \" "
    ++ "<< err.what() << \"\\n\\n\";"
  , "                errorcount += errorcount != 0x7FFFFFFF;"
  , "            },"
  , "            [](auto&& ast) {"
  , "                if (systest) {"
  , "                    cout << \"Parse Successful!\\n\\n"
    ++ "[Abstract Syntax]\\n\\n\";"
  , "                    ast | " ++ ns ++ "HaskellPrinter(cout);"
  , "                    cout << \"\\n\\n[Linearized tree]\\n\\n\";"
  , "                    ast | " ++ ns ++ "PrettyPrinter(cout);"
  , "                    cout << endl;"
  , "                    return;"
  , "                }"
  , "                bool printed = false;"
  , "                if (tree) {"
  , "                    ast | " ++ ns ++ "SyntaxPrinter(cout);"
  , "                    cout << '\\n';"
  , "                    printed = true;"
  , "                }"
  , "                if (pretty) {"
  , "                    ast | " ++ ns ++ "PrettyPrinter(cout);"
  , "                    cout << \"\\n\\n\";"
  , "                    printed = true;"
  , "                }"
  , "                if (haskell) {"
  , "                    ast | " ++ ns ++ "HaskellPrinter(cout);"
  , "                    cout << \"\\n\\n\";"
  , "                    printed = true;"
  , "                }"
  ] $+$ unlinesToText [s|
                if (!printed) cout << "OK\n\n";
                return;
            }
        };

        if (needclose) fclose(file);
    }

    return errorcount;
}
|]
