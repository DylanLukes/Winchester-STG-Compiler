#include <memory>
#include <string>
#include <iostream>
#include <syntax/lexer.hh>

using namespace std;
using namespace wsc::lex;

int main (int argc, const char * argv[])
{
  Lexer l;

  const char *text = "λx.x+x";

  l.lex(text, strlen(text));

  cout << endl;

  return 0;
}

