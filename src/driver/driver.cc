#include <memory>
#include <string>
#include <iostream>
#include <syntax/ast.h>

using namespace std;

int main (int argc, const char * argv[])
{
  using namespace wsc::syntax::ast;
  
  unique_ptr<Variable> v1(new Variable("foo"));
  unique_ptr<IntegralLiteral> l1(new IntegralLiteral(13));
  
  vector<unique_ptr<Argument>> args;
  
  unique_ptr<VariableArgument> a1(new VariableArgument(move(v1)));
  unique_ptr<LiteralArgument> a2(new LiteralArgument(move(l1))); 
  
  args.push_back(move(a1));
  args.push_back(move(a2));
  
  return 0;
}

