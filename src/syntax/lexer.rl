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

Lexer::Lexer(string filename) : in_file(filename.c_str(), ifstream::in), in_buf(new char[BUF_SIZE]) {
  // initialize lemon parser (maybe pass in instead?)
  %% write init;
}

Lexer::~Lexer() {
  // dealloc lemon parser
}

// maybe I should work a stringbuf backed stream in here instead
int Lexer::lex() {
  bool done = false;
  int space = 0, have = 0, len = 0;

  while(!done){
    space = BUF_SIZE - have;
    if (space == 0) {
      // Buffer is full
      cerr << "LEX ERROR: Token too big." << endl;
      return -1;
    }

    // Read in a block of data (offset to after whatever already have)
    len = in_file.readsome(in_buf.get() + have, space);

    const char *p = in_buf.get() + have;
    const char *pe = p + len;
    const char *eof = 0;

    // Check for EOF
    if(len == 0) {
      eof = pe;
      done = true;
        // Note: EOF doesn't mean we don't have more to lex!
        // We'll let the roof run it's course.
    }

    %% write exec;

    if(cs == wsc_lex_error) {
      // Machine failed before finding a token
      cerr << "LEX ERROR: Failed before finding a token." << endl;
      return -1;
    }

    if(ts == 0) {
      have = 0;
    } else {
      // There is a prefix to preserve, shift it over.
      // Note how much we've kept for use as an offset.
      have = pe - ts;
      memmove(in_buf.get(), ts, have);
      te = in_buf.get() + (te-ts);
      ts = in_buf.get();
    }
  }
  return 0;
}
