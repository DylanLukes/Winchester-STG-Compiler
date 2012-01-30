#include "ast.hh"

using namespace wsc::ast;

Name *Name::clone() const { return new Name(*this); }
void Name::accept(Visitor &v) { v.visit(*this); }
void Name::display(std::ostream &o) const { o << "Name[str=" << str << "]"; }
