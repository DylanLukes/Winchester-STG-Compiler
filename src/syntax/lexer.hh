#pragma once

#include <fstream>
#include <memory>

namespace wsc {
  namespace lex {
    enum Token {
      TOK_PLUS,
      TOK_MINUS,
      TOK_NUM
    };

    class Lexer {
    public:
      Lexer(std::string filename);
      ~Lexer();
    
      int lex();
    private:
      std::ifstream in_file;
      std::unique_ptr<char[]> in_buf;

      int cs, act;
      const char *ts, *te;
      void *lemon_parser;
    };
  }
}
