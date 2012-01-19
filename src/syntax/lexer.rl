#include <iostream>
#include <string>
#include <cstdlib>

#include "lexer.hh"

using namespace std;
using namespace wsc::lex;

%%{
  machine wsc_lex;
      
  main := |*
    ([a-zA-Z_][a-zA-Z0-9_]*)  => {cout << "IDENT, "; };
    ([0-9]+('.'[0-9]+)?)      => {cout << "NUM, ";   };
    '.'                       => {cout << "DOT, ";   };
    '+'                       => {cout << "ADD, ";   };
    '-'                       => {cout << "SUB, ";   };
    "λ"                       => {cout << "LAMDA, "; };
  *|;
      
}%%

%% write data;

Lexer::Lexer() {
  // initialize lemon parser (maybe pass in instead?)

  %% write init;
}

Lexer::~Lexer() {
  // dealloc lemon parser
}

// maybe I should work a stringbuf backed stream in here instead
void Lexer::lex(const char *data, size_t len) {
  const char* p = data;
  const char* pe = p + len;
  const char* eof = pe;

  %% write exec;

  // handle errors and left overs
}
