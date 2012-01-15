#pragma once

#include <string>
#include <memory>
#include <vector>

using namespace std;

namespace wsc {
  namespace syntax {
    namespace ast {

      //typedef string Variable;

      /* Forward Declarations */

      class ASTVisitor;
      /* abstract */ class ASTNode;

      /* concrete */ class Alternative;
      
      /* abstract */ class Argument;
      /* concrete */ class VariableArgument;
      /* concrete */ class LiteralArgument;

      /* abstract */ class Binding;
      /* concrete */ class NonRecursiveBinding;
      /* concrete */ class RecursiveBinding;

      /* abstract */ class Expression;
      /* concrete */ class ApplicationExpression;
      /* concrete */ class LiteralExpression;
      /* concrete */ class LetExpression;
      /* concrete */ class CaseExpression;

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
      
      /* concrete */ class Variable;

      class Literal;

      class ASTVisitor {
      public:
        virtual void visit(VariableArgument      &visitee) {}
        virtual void visit(LiteralArgument       &visitee) {}
        
        virtual void visit(NonRecursiveBinding   &visitee) {}
        virtual void visit(RecursiveBinding      &visitee) {}
        
        virtual void visit(ApplicationExpression &visitee) {}
        virtual void visit(LiteralExpression     &visitee) {}
        virtual void visit(LetExpression         &visitee) {}
        virtual void visit(CaseExpression        &visitee) {}
        
        virtual void visit(AlgebraicPattern      &visitee) {}
        virtual void visit(LiteralPattern        &visitee) {}
        virtual void visit(DefaultPattern        &visitee) {}
        
        virtual void visit(IntegralLiteral       &visitee) {}
        virtual void visit(RealLiteral           &visitee) {}
        
        virtual void visit(Variable              &visitee) {}
        
        virtual void visit(ClosureRValue         &visitee) {}
        virtual void visit(DataConRValue         &visitee) {}
      };

      class ASTNode {
      public:
        virtual ~ASTNode() {}
        virtual void accept(ASTVisitor &v) = 0;
      };

      class Argument : public ASTNode {
      };
      
      class VariableArgument final : public Argument {
      public:
        unique_ptr<Variable> var;
        
        VariableArgument(unique_ptr<Variable> var) : var(move(var)) {}
        void accept(ASTVisitor &v) { v.visit(*this); }        
      };
      
      class Literal : public ASTNode {
        
      };
      
      class LiteralArgument final : public Argument {
      public:
        unique_ptr<Literal> literal;
        
        LiteralArgument(unique_ptr<Literal> literal) : literal(move(literal)) {}
        void accept(ASTVisitor &v) { v.visit(*this); } 
      };
      
      class IntegralLiteral final : public Literal {
      public:
        int val;
        
        IntegralLiteral(int val) : val(val) {}
        void accept(ASTVisitor &v) { v.visit(*this); }
      };
      
      class RealLiteral final : public Literal {
      public:
        float val;
        
        RealLiteral(float val) : val(val) {}
        void accept(ASTVisitor &v) { v.visit(*this); }
      };
      
      class Variable final : public ASTNode {
      public:
        string name;
        
        Variable(string name) : name(name) {}
        void accept(ASTVisitor &v) { v.visit(*this); }
      };
      
    }
  }
}
