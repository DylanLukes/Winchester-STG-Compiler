#include "ast.h"

using namespace wsc::ast;

void Alternative::accept(Visitor &v) {
  v.visit(*this);
  //pattern->accept(v);
  //expression->accept(v);
}

void VariableArgument::accept(Visitor &v) {
  v.visit(*this);
  variable->accept(v);
}

void LiteralArgument::accept(Visitor &v) {
  v.visit(*this);
  literal->accept(v);
}

void Binding::accept(Visitor &v) {
  v.visit(*this);
  binder->accept(v);
  bindee->accept(v);
}

void ApplicationExpression::accept(Visitor &v) {
  v.visit(*this);
  function->accept(v);
  for (shared_ptr<Argument> arg : arguments) {
    arg->accept(v);
  }
}

void LiteralExpression::accept(Visitor &v) {
  v.visit(*this);
  literal->accept(v);
}

void LetExpression::accept(Visitor &v) {
  v.visit(*this);
  for (shared_ptr<Binding> binding : bindings) {
    binding->accept(v);
  }
  expression->accept(v);
}

void CaseExpression::accept(Visitor &v) {
  v.visit(*this);
  scrutinee->accept(v);
  case_binder->accept(v);
  for (shared_ptr<Alternative> alt : alternatives) {
    alt->accept(v);
  }
}

void Identifier::accept(Visitor &v) {
  v.visit(*this);
}

void IntegralLiteral::accept(Visitor &v) {
  v.visit(*this);
}

void RealLiteral::accept(Visitor &v) {
  v.visit(*this);
}

void AlgebraicPattern::accept(Visitor &v) {
  v.visit(*this);
  constructor->accept(v);
  for (shared_ptr<Identifier> binder : binders) {
    binder->accept(v);
  }
}

void LiteralPattern::accept(Visitor &v) {
  v.visit(*this);
  literal->accept(v);
}

void DefaultPattern::accept(Visitor &v) {
  v.visit(*this);
}

void ClosureRValue::accept(Visitor &v) {
  v.visit(*this);
  for (shared_ptr<Identifier> binder : binders) {
    binder->accept(v);
  }
  expression->accept(v);
}

void DataConRValue::accept(Visitor &v) {
  v.visit(*this);
  data_constructor->accept(v);
  for (shared_ptr<Argument> arg : arguments) {
    arg->accept(v);
  }
}

