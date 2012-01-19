#pragma once

#include <fstream>
#include <memory>

using namespace std;

namespace wsc {
	namespace lex {
		enum Token {
			TOK_PLUS,
      TOK_MINUS,
      TOK_NUM
		};

    class Lexer {
    public:
      Lexer(string filename);
      ~Lexer();
    
    int lex();

    private:
      ifstream in_file;
      unique_ptr<char[]> in_buf;

    	int cs, act;
    	const char *ts, *te;
    	void *lemon_parser;
    };
  }
}
