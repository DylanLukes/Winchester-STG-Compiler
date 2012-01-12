namespace wsc {
	namespace front {
		
		class Lexer : public yyFlexLexer{
		public:
			int yylex();
		};
		
	}
}
