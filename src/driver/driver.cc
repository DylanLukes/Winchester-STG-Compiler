#include <iostream>
#include <memory>

#include "syntax/ast.hh"

using namespace std;
using namespace wsc::ast;

int level = 0;

void printNode(Node &a) {
  cout << &a << ": " << a << endl; 
}

int main (int argc, const char * argv[]) {  
  auto lit = make_shared<IntegralLiteral>(13);
  auto litexpr = make_shared<LiteralExpression>(lit);

  auto opnd = make_shared<LiteralOperand>(lit);

  auto bndr = make_shared<Binder>("foo");
  // auto bnde = make_shared<ClosureBindee>(...);

  // auto lexpr = make_shared<LetExpression>(bndr, ...);

  Visitor v;
  v.visitBinder = printNode;

  bndr->accept(v);

  return 0;
}
