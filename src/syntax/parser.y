%name wsc_parser_

%include {
	#include <iostream>
	#include "ast.hh"

	using namespace std;
	using namespace wsc;
}

%token_type {ast::Node}
%token_prefix TOKEN_

%syntax_error {
	cerr << "Syntax error." << endl;
}

program ::= literal. { cout << "Done." << endl; }

%type literal {shared_ptr<ast::Literal>}
literal(A) ::= TOKEN_INT(B). {cout << "A: " << A << " B: " << B << endl; }