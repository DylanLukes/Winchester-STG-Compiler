#pragma once

#include <memory>
#include <string>
#include <vector>

namespace wsc {
  namespace ast {
    /* Forward Declarations */
    /* abstract */ class Node;
    /* concrete */ class Visitor;
    
    /* concrete */ class Alternative;
      
    /* abstract */ class Argument;
    /* concrete */ class VariableArgument;
    /* concrete */ class LiteralArgument;

    /* concrete */ class Binding;

    /* abstract */ class Expression;
    /* concrete */ class ApplicationExpression;
    /* concrete */ class LiteralExpression;
    /* concrete */ class LetExpression;
    /* concrete */ class CaseExpression;

    /* concrete */ class Identifier;
    
    /* abstract */ class Literal;
    /* concrete */ class IntegralLiteral;
    /* concrete */ class RealLiteral;
      
    /* abstract */ class Pattern;
    /* concrete */ class AlgebraicPattern;
    /* concrete */ class LiteralPattern;
    /* concrete */ class DefaultPattern;

    /* abstract */ class RValue;
    /* concrete */ class ClosureRValue;
    /* concrete */ class DataConRValue;
    
    class Visitor {
    public:            
      virtual void visit(Alternative           &a) {}
      
      /* Arguments */
      virtual void visit(VariableArgument      &a) {}
      virtual void visit(LiteralArgument       &a) {}
      
      virtual void visit(Binding               &a) {}
      
      /* Expressions */
      virtual void visit(ApplicationExpression &a) {}
      virtual void visit(LiteralExpression     &a) {}
      virtual void visit(LetExpression         &a) {}
      virtual void visit(CaseExpression        &a) {}
      
      virtual void visit(Identifier            &a) {}
      
      /* Literals */
      virtual void visit(IntegralLiteral       &a) {}
      virtual void visit(RealLiteral           &a) {}
      
      /* Patterns */
      virtual void visit(AlgebraicPattern      &a) {}
      virtual void visit(LiteralPattern        &a) {}
      virtual void visit(DefaultPattern        &a) {}
      
      /* RValues */
      virtual void visit(ClosureRValue         &a) {}
      virtual void visit(DataConRValue         &a) {}
      
    };
    
    class Node {
    public:
      virtual void accept(Visitor &v) = 0;
    };
    
    class Alternative final : Node {
    public:
      std::shared_ptr<Pattern> pattern;
      std::shared_ptr<Expression> expression;
      
      Alternative(std::shared_ptr<Pattern> pattern, std::shared_ptr<Expression> expression) : pattern(pattern), expression(expression) {}
      void accept(Visitor &v) override;
    };
    
    class Argument : public Node {};
    
    class VariableArgument final : public Argument {
    public:
      std::shared_ptr<Identifier> variable;
      
      VariableArgument(std::shared_ptr<Identifier> variable) : variable(variable) {}
      void accept(Visitor &v) override;
    };
    
    class LiteralArgument final : public Argument {
    public:
      std::shared_ptr<Literal> literal;
      
      LiteralArgument(std::shared_ptr<Literal> literal) : literal(literal) {}
      void accept(Visitor &v) override;
    };
        
    class Binding final : public Node {
    public:
      std::shared_ptr<Identifier> binder;
      std::shared_ptr<RValue> bindee;
      
      Binding(std::shared_ptr<Identifier> binder, std::shared_ptr<RValue> bindee) : binder(binder), bindee(bindee) {}
      void accept(Visitor &v) override;
    };
    
    class Expression : public Node {};
    
    class ApplicationExpression final : public Expression {
    public:
      std::shared_ptr<Identifier> function;
      std::vector<std::shared_ptr<Argument>> arguments;
      
      ApplicationExpression(std::shared_ptr<Identifier> function, std::vector<std::shared_ptr<Argument>> arguments) : function(function), arguments(arguments) {}
      void accept(Visitor &v) override;
    };
    
    class LiteralExpression final : public Expression {
    public:
      std::shared_ptr<Literal> literal;
      
      LiteralExpression(std::shared_ptr<Literal> literal) : literal(literal) {}
      void accept(Visitor &v) override;
    };
    
    class LetExpression final : public Expression {
    public:
      std::vector<std::shared_ptr<Binding>> bindings;
      std::shared_ptr<Expression> expression;
      
      LetExpression(std::vector<std::shared_ptr<Binding>> bindings, std::shared_ptr<Expression> expression) : bindings(bindings), expression(expression) {}
      void accept(Visitor &v) override;
    };
    
    class CaseExpression final : public Expression {
    public:
      std::shared_ptr<Expression> scrutinee;
      std::shared_ptr<Identifier> case_binder;
      std::vector<std::shared_ptr<Alternative>> alternatives;
      
      CaseExpression(std::shared_ptr<Expression> scrutinee, std::shared_ptr<Identifier> case_binder, std::vector<std::shared_ptr<Alternative>> alternatives) : scrutinee(scrutinee), case_binder(case_binder), alternatives(alternatives) {}
      void accept(Visitor &v) override;
    };
    
    class Identifier final : public Node {
    public:
      std::string name;
      
      Identifier(std::string name) : name(name) {}
      void accept(Visitor &v) override; 
    };
    
    class Literal : public Node {};
    
    class IntegralLiteral final : public Literal {
    public:
      int value;
      
      IntegralLiteral(int value) : value(value) {}
      void accept(Visitor &v) override;
    };
    
    class RealLiteral final : public Literal {
    public:
      float value;
      
      RealLiteral(float value) : value(value) {}
      void accept(Visitor &v) override;
    };
    
    class Pattern : public Node {};
    
    class AlgebraicPattern final : public Pattern {
    public:
      std::shared_ptr<Identifier> constructor;
      std::vector<std::shared_ptr<Identifier>> binders;
      
      AlgebraicPattern(std::shared_ptr<Identifier> constructor, std::vector<std::shared_ptr<Identifier>> binders) : constructor(constructor), binders(binders) {}
      void accept(Visitor &v) override;
    };
    
    class LiteralPattern final : public Pattern {
    public:
      std::shared_ptr<Literal> literal;
      
      LiteralPattern(std::shared_ptr<Literal> literal) : literal(literal) {}
      void accept(Visitor &v) override;
    };
    
    class DefaultPattern final : public Pattern {
    public:
      DefaultPattern() {}
      void accept(Visitor &v) override;
    };
    
    class RValue : public Node {};
    
    class ClosureRValue final : public RValue {
    public:
      enum UpdateFlag { UPDATABLE, REENTRANT } update_flag;
      std::vector<std::shared_ptr<Identifier>> binders;
      std::shared_ptr<Expression> expression;
      
      ClosureRValue(UpdateFlag update_flag, std::vector<std::shared_ptr<Identifier>> binders, std::shared_ptr<Expression> expression) : update_flag(update_flag), binders(binders), expression(expression) {}
      void accept(Visitor &v) override;
    };
    
    class DataConRValue final : public RValue {
    public:
      std::shared_ptr<Identifier> data_constructor;
      std::vector<std::shared_ptr<Argument>> arguments;
      
      DataConRValue(std::shared_ptr<Identifier> data_constructor, std::vector<std::shared_ptr<Argument>> arguments) : data_constructor(data_constructor), arguments(arguments) {}
      void accept(Visitor &v) override;
    };
  }
}
