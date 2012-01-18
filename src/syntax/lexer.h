namespace wsc {
	namespace lex {
		enum Token {
			TOK_PLUS,
      TOK_MINUS,
      TOK_NUM
		};

    class Lexer {
    public:
      Lexer();
      ~Lexer();
      
    void lex(const char *data, size_t len);
    private:
    	int cs, act;
    	const char *ts, *te;
			
    	void *lemon_parser;
    };
  }
}
