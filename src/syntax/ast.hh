#pragma once

#include <iostream>
#include <functional>
#include <memory>
#include <string>

namespace wsc {
  namespace ast {

    /* abstract */ class Node;
    /* concrete */ class Binder;
    /* concrete */ class Occurrence;

    /* Visitor */

    class Visitor {
    public:
      std::function<void (Binder &)>     visitBinder;
      std::function<void (Occurrence &)> visitOccurrence;
    };

    /* Node */
    // Node is the base class of all abstract syntax nodes.

    class Node {
    public:
      virtual Node *clone() const = 0;
      virtual void accept(Visitor &) = 0;

      friend std::ostream &operator<<(std::ostream &o, Node const &node) {
        node.display(o); return o;
      }      
    protected:
      virtual void display(std::ostream &) const = 0;
    };

    /* Binder */
    // Binder represents the binding site of an identifier.

    class Binder final : public Node {
    public:
      Binder(std::string name) : name(name) {}
      Binder(const Binder &);

      Binder *clone() const override;
      void accept(Visitor &) override;

      std::string name;
    private:
      void display(std::ostream &) const override;
    };

    /* Occurrence */
    // An Occurence is just a redirection to its Binder.

    class Occurrence : public Node {
    public:
      Occurrence(std::shared_ptr<Binder> origin) : origin(origin) {}
      Occurrence(const Occurrence &);

      Occurrence *clone() const override;
      void accept(Visitor &) override;

      std::shared_ptr<Binder> origin;
    private:
      void display(std::ostream &) const override;
    };
  }
}
