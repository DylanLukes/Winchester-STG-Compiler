#include "ast.hh"

using namespace wsc::ast;

#define CLONE_METHOD_DEF(T) \
T *T::clone() const {       \
  return new T(*this);      \
}

#define ACCEPT_METHOD_DEF(T) \
void T::accept(Visitor &v) { \
  v.visit##T(*this);         \
}

#define DISPLAY_METHOD_DEF(T, E)         \
void T::display(std::ostream &o) const { \
  E;                                     \
}

/* LiteralOperand */

CLONE_METHOD_DEF(LiteralOperand);
ACCEPT_METHOD_DEF(LiteralOperand);
DISPLAY_METHOD_DEF(LiteralOperand, 
  o << "LiteralOperand[literal=" << literal << "]");

/* Binder */

CLONE_METHOD_DEF(Binder);
ACCEPT_METHOD_DEF(Binder);
DISPLAY_METHOD_DEF(Binder,
  o << "Binder[name=" << name << "]");

/* Binding */
CLONE_METHOD_DEF(Binding);
ACCEPT_METHOD_DEF(Binding);
DISPLAY_METHOD_DEF(Binding,
  o << "Binding[binder=" << binder << ", bindee=" << bindee << "]");

/* LetExpression */

CLONE_METHOD_DEF(LetExpression);
ACCEPT_METHOD_DEF(LetExpression);
DISPLAY_METHOD_DEF(LetExpression,
  o << "LetExpression[binding=" << binding << ", expression=" << expression << "]");

/* LiteralExpression */

CLONE_METHOD_DEF(LiteralExpression);
ACCEPT_METHOD_DEF(LiteralExpression);
DISPLAY_METHOD_DEF(LiteralExpression,
  o << "LiteralExpression[literal=" << literal << "]");


/* IntLiteral */

CLONE_METHOD_DEF(IntegralLiteral);
ACCEPT_METHOD_DEF(IntegralLiteral);
DISPLAY_METHOD_DEF(IntegralLiteral,
  o << "IntegralLiteral[value=" << value << "]");

/* Occurrence */

CLONE_METHOD_DEF(LocalOccurrence);
ACCEPT_METHOD_DEF(LocalOccurrence);
DISPLAY_METHOD_DEF(LocalOccurrence,
  o << "LocalOccurrence[binder=" << origin << "]");
