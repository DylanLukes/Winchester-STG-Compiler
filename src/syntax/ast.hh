#pragma once

#include <iostream>
#include <functional>
#include <memory>
#include <string>

namespace wsc {
  namespace ast {

    class Node;
    class Name;
    class Occurrence;

    /* Visitor */
    inline void do_nothing(Node &) {} ;

    struct Visitor {
      void visit(Name &);
    };

    /** 
     * Base class for all abstract syntax tree nodes.
     */
    class Node {
    public:
      virtual Node *clone() const = 0;
      virtual void accept(Visitor &) = 0;

      friend std::ostream &operator<<(std::ostream &o, Node const &node);
      friend struct Visitor;
    protected:
      virtual void display(std::ostream &) const = 0;
    };

    std::ostream &operator<<(std::ostream &o, Node const &node) {
      node.display(o);
      return o;
    }

    class Name : public Node {
    public:
      Name *clone() const override;
      void accept(Visitor &v) override;

      std::string str;
    private:
      void display(std::ostream &) const;
    };


  }
}
