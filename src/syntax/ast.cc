#include "ast.hh"

/* Binder */

using namespace wsc::ast;


Binder::Binder(const Binder &other) {
  name = other.name;
}

Binder *Binder::clone() const { 
  return new Binder(*this);
}

void Binder::accept(Visitor &v) {
  v.visitBinder(*this);
}

void Binder::display(std::ostream &o) const {
  o << "Binder[name=" << name << "]";
}

/* Occurrence */

Occurrence::Occurrence(const Occurrence &other) {
  origin = other.origin;
}

Occurrence *Occurrence::clone() const {
  return new Occurrence(*this);
}

void Occurrence::accept(Visitor &v) {
  v.visitOccurrence(*this);
}

void Occurrence::display(std::ostream &o) const {
  o << "Occurrence[origin.name=" << origin->name << "]";
}
