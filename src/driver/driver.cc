#include <iostream>
#include <memory>

// #include "syntax/ast.hh"

using namespace std;
//using namespace wsc::ast;

int main (int argc, const char * argv[]) {
  shared_ptr<string> a(new string("foo"));
  return 0;
}
