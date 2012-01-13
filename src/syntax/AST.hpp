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
            
            /* concrete */ class Alternative;
            
            /* abstract */ class Pattern;
            /* concrete */ class AlgebraicPattern;
            /* concrete */ class LiteralPattern;
            /* concrete */ class DefaultPattern;
            
            /* abstract */ class Literal;
            /* concrete */ class IntegerLiteral;
            
            /* abstract */ class RValue;
            /* concrete */ class ClosureRValue;
            /* concrete */ class DataConRValue;
            
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
                virtual void visit(IntegerLiteral        &visitee) {}
                virtual void visit(ClosureRValue         &visitee) {}
                virtual void visit(DataConRValue         &visitee) {}
            };
            
            class ASTNode {
            public:
                virtual ~ASTNode() {}
                virtual void accept(ASTVisitor &v) = 0;
            };
            
            /* Arguments */
            
            class Argument : public ASTNode {
            public:
                virtual ~Argument() {}
            };
            
            class VariableArgument final : public Argument {
                string mVariable;
            public:
                VariableArgument(string variable) :
                    mVariable(variable) {}
                ~VariableArgument() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            class LiteralArgument : public Argument {
                unique_ptr<Literal> mLiteral;
            public:
                LiteralArgument(unique_ptr<Literal> literal) :
                    mLiteral(move(literal)) {}
                ~LiteralArgument() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            /* Bindings */
            
            class Binding : public ASTNode {
            public:
                virtual ~Binding() {}
            };
            
            class NonRecursiveBinding : public Binding {
                string mBinder;
                unique_ptr<RValue> mRValue;
            public:
                NonRecursiveBinding(string binder, unique_ptr<RValue> rValue) : 
                    mBinder(binder),
                    mRValue(move(rValue)) {}
                ~NonRecursiveBinding() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            class RecursiveBinding : public Binding {
                vector<NonRecursiveBinding> mBindings;
            public:
                RecursiveBinding(vector<NonRecursiveBinding> bindings) :
                    mBindings(move(bindings)) {} // this line throws up
                ~RecursiveBinding() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            /* Expressions */
            
            class Expression : public ASTNode {
            public:
                virtual ~Expression() {}
            };
            
            class LiteralExpression : public Expression {
                unique_ptr<Literal> mLiteral;
            public:
                LiteralExpression(unique_ptr<Literal> literal) :
                    mLiteral(move(literal)) {}
                ~LiteralExpression() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            /* Literals */
            
            class Literal : public ASTNode {
            public:
                virtual ~Literal() {}
            };
            
            class IntegerLiteral : public Literal {
                int mValue;
            public:
                IntegerLiteral(int value) : mValue(value) {}
                ~IntegerLiteral() {}
                
                void accept(ASTVisitor &v) { v.visit(*this); }
            };
            
            /* RValues */
            
            class RValue : public ASTNode {
            public:
                virtual ~RValue() {}
            };
            
        }
    }
}
