#include <iostream>
#include <string>
#include <cstdlib>

#include "lexer.h"

using namespace std;
using namespace wsc::lex;

%%{
  machine wsc_lex;
      
  action tok_number {
    cout << "NUM";
  }
      
  action tok_plus {
    cout << "PLUS";
  }
      
  action tok_minus {
    cout << "MINUS";
  }
      
  number = [0-9]+('.'[0-9]+)?;
  plus  = '+';
  minus = '-';
      
  main := |*
    number => tok_number;
    plus => tok_plus;
    minus => tok_minus;
  *|;
      
}%%

%% write data;

Lexer::Lexer() {
  %% write init;
}

Lexer::~Lexer() {
  
}

void Lexer::lex(const char *data, size_t len) {
  const char* p = data;
  const char* pe = data + len;
  const char* eof = pe;

  %% write exec;
}
