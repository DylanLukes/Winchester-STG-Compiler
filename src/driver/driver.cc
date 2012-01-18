#include <memory>
#include <string>
#include <iostream>
#include <syntax/ast.hh>

using namespace std;
using namespace wsc::ast;

class TestVisitor final : public Visitor {  
  void visit(Identifier &a) {
    cout << "Visiting identifier: " << a.name << endl;
  }
};

int main (int argc, const char * argv[])
{
  shared_ptr<Identifier> v(new Identifier("foo"));
  shared_ptr<VariableArgument> va(new VariableArgument(v));
    
  shared_ptr<Argument> a(va);
  
  TestVisitor vs;
  
  a->accept(vs);
  
  return 0;
}

