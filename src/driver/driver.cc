#include <memory>
#include <string>
#include <iostream>
#include <fstream>
#include "syntax/lexer.hh"

using namespace std;
using namespace wsc::lex;

int main (int argc, const char * argv[])
{
  Lexer l("bin/test");
  l.lex();

  cout << endl;

  return 0;
}

