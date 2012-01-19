#include "lexer.hh"

using namespace std;
using namespace wsc::lex;

#define BUF_SIZE (4096)

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

Lexer::Lexer(string filename) : in_file(filename.c_str(), ifstream::in), in_buf((char *)malloc(BUF_SIZE), free) {

  // initialize lemon parser (maybe pass in instead?)
  %% write init;
}

Lexer::~Lexer() {
  // dealloc lemon parser
}

// maybe I should work a stringbuf backed stream in here instead
void Lexer::lex() {
  int bytes_read = in_file.readsome(in_buf.get(), BUF_SIZE);

  const char* p   = in_buf.get();
  const char* pe  = p + bytes_read;
  const char* eof = pe;

  %% write exec;

  // handle errors and left overs
}
