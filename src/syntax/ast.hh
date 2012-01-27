#pragma once

#include <iostream>
#include <functional>
#include <memory>
#include <string>

#include <llvm/Value.h>

namespace wsc {
  namespace ast {

    /* abstract */ class Node;
    /* abstract */ class Bindee;
    /* concrete */ class Binder;
    /* concrete */ class Binding;
    /* abstract */ class Expression; // TODO
      /* concrete */ class LetExpression;
      /* concrete */ class LiteralExpression;
    /* abstract */ class Literal;
    /* concrete */   class IntegralLiteral;
    /* abstract */ class Occurrence;
      /* concrete */ class LocalOccurrence;
    /* abstract */ class Operand;
      /* concrete */   class LiteralOperand;

    /* Visitor */
    inline void do_nothing(Node &) {} ;

    struct Visitor {
      Visitor() : visitBinder            (do_nothing)
                , visitBinding           (do_nothing)
                , visitLetExpression     (do_nothing)
                , visitLiteralExpression (do_nothing)
                , visitIntegralLiteral   (do_nothing)
                , visitLocalOccurrence   (do_nothing)
                , visitLiteralOperand    (do_nothing) {}

      std::function<void (Binder &)>            visitBinder;
      std::function<void (Binding &)>           visitBinding;
      std::function<void (LetExpression &)>     visitLetExpression;
      std::function<void (LiteralExpression &)> visitLiteralExpression;
      std::function<void (IntegralLiteral &)>   visitIntegralLiteral;
      std::function<void (LocalOccurrence &)>   visitLocalOccurrence;
      std::function<void (LiteralOperand &)>    visitLiteralOperand;
    };

    /* Abstract Base Class */ 

    template <class T>
    class Node {
    public:
      virtual Node *clone() const = 0;
      virtual void accept(Visitor &) = 0;

      friend llvm::Value *codegen(Node &);
      friend std::ostream &operator<<(std::ostream &o, Node const &node) {
        node.display(o); return o;
      }

      friend struct Visitor;
    protected:
      virtual void display(std::ostream &) const = 0;
    };

    /* Classes */

    class Bindee : public Node {
      std::weak_ptr<Binding> binding;
    };

    class Binder : public Node {
    public:
      Binder(std::string name) : name(name) {}

      Binder *clone() const override;
      void accept(Visitor &) override;

      std::string name;
      std::weak_ptr<Binding> binding;
    private:
      void display(std::ostream &) const override;
    };

    class Binding : public Node {
    public:
      Binding(std::shared_ptr<Binder> binder, std::shared_ptr<Bindee> bindee)
        : binder(binder)
        , bindee(bindee) {}

      Binding *clone() const override;
      void accept(Visitor &) override;
    
      std::shared_ptr<Binder> binder;
      std::shared_ptr<Bindee> bindee;
    private:
      void display(std::ostream &) const override;
    };

    class Expression : public Node {};

    class LetExpression : public Expression {
    public:
      LetExpression(std::shared_ptr<Binding> binding, std::shared_ptr<Expression> expression)
        : binding(binding)
        , expression(expression) {}
    
      LetExpression *clone() const override;
      void accept(Visitor &) override;

      std::shared_ptr<Binding> binding;
      std::shared_ptr<Expression> expression;
    private:
      void display(std::ostream &) const override;
    };

    class LiteralExpression : public Expression {
    public:
      LiteralExpression(std::shared_ptr<Literal> literal)
        : literal(literal) {}
    
      LiteralExpression *clone() const override;
      void accept(Visitor &) override;

      std::shared_ptr<Literal> literal;
    private:
      void display(std::ostream &) const override;
    };

    class Literal : public Node {};

    class IntegralLiteral : public Literal {
    public:
      IntegralLiteral(int value) : value(value) {}

      IntegralLiteral *clone() const override;
      void accept(Visitor &) override;

      int value;
    private:
      void display(std::ostream &) const override;
    };

    class Occurrence : public Node {};

    class LocalOccurrence : public Occurrence {
    public:
      LocalOccurrence(std::shared_ptr<Binder> binder) : binder(binder) {}

      LocalOccurrence *clone() const override;
      void accept(Visitor &) override;

      std::shared_ptr<Binder> binder;
    private:
      void display(std::ostream &) const override;
    };

    class Operand : public Node {};

    class LiteralOperand : public Operand {
    public:
      LiteralOperand(std::shared_ptr<Literal> literal) 
        : literal(literal) {}

      LiteralOperand *clone() const override;
      void accept(Visitor &) override;

      std::shared_ptr<Literal> literal;
    private:
      void display(std::ostream &) const override;
    };
  }
}
