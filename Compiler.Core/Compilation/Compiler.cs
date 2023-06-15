using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Compiler.Core.Compilation.IR;
using Compiler.Core.Compilation.IR.Instructions;
using Compiler.Core.Compilation.IR.Prototypes;
using Compiler.Core.Exceptions;
using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.CodeAnalysis.Lua.Syntax;

namespace Compiler.Core.Compilation;

internal sealed class Compiler
{
    public static Module Compile(SyntaxTree syntaxTree) => new Compiler(new Module()).CompileMainPrototype(syntaxTree);

    private readonly Module module;
    private readonly Prototype prototype;

    private class Scope
    {
        public Scope(Compiler compiler)
        {
            OldTop = compiler.Top;
            _compiler = compiler;
            VariableMapping = new();
        }

        public int OldTop;
        public Compiler _compiler;

        public readonly Dictionary<string, int> VariableMapping;
    }

    private Compiler(Module module)
    {
        this.module = module;
        
        prototype = new Prototype();
        AvailableHeapValues = new Dictionary<string, int>();
    }

    private Module CompileMainPrototype(SyntaxTree tree)
    {
        module.SoftPrototypes.Add(prototype);
        
        var compilationUnit = (CompilationUnitSyntax)tree.GetRoot();
        
        var mainScope = new Scope(this);
        
        foreach (var statement in compilationUnit.Statements.Statements)
        {
            CompileStatement(statement, mainScope);
        }

        prototype.EmitInstruction(Operation.OP_RETURN, 0, 1);

        return module;
    }

    public Prototype CompileChildPrototype()
    {
        return new Prototype();
    }

    private void CompileStatement(StatementSyntax statement, Scope scope)
    {
        switch (statement)
        {
            case ExpressionStatementSyntax expressionStatement:
            {
                CompileExpression(expressionStatement.Expression, scope);
                break;
            }
            case LocalVariableDeclarationStatementSyntax localVariableDeclarationStatement:
            {
                CompileLocalVariableDeclarationStatement(localVariableDeclarationStatement, scope);
                break;
            }
            case AssignmentStatementSyntax assignmentStatement:
            {
                CompileAssignmentStatement(assignmentStatement, scope);
                break;
            }
            case CompoundAssignmentStatementSyntax compoundAssignmentStatement:
            {
                CompileCompoundAssignmentStatement(compoundAssignmentStatement, scope);
                break;
            }
            case ReturnStatementSyntax returnStatement:
            {
                CompileReturnStatement(returnStatement, scope);
                break;
            }
            case DoStatementSyntax doStatement:
            {
                CompileDoStatement(doStatement, scope);
                break;
            }
            case IfStatementSyntax ifStatement:
            {
                CompileIfStatement(ifStatement, scope);
                break;
            }
            case NumericForStatementSyntax numericForStatement:
            {
                CompileNumericForStatement(numericForStatement, scope);
                break;
            }
            case GenericForStatementSyntax genericForStatement:
            {
                CompileGenericForStatement(genericForStatement, scope);
                break;
            }
            case LocalFunctionDeclarationStatementSyntax localFunctionDeclarationStatement:
            {
                CompileLocalFunctionDeclarationStatement(localFunctionDeclarationStatement, scope);
                break;
            }
            case FunctionDeclarationStatementSyntax functionDeclarationStatement:
            {
                CompileFunctionDeclarationStatement(functionDeclarationStatement, scope);
                break;
            }
            case WhileStatementSyntax whileStatementSyntax:
            {
                CompileWhileStatement(whileStatementSyntax, scope);
                break;
            }
            case RepeatUntilStatementSyntax repeatUntilStatement:
            {
                CompileRepeatUntilStatement(repeatUntilStatement, scope);
                break;
            }
            case ContinueStatementSyntax continueStatement:
            {
                CompileContinueStatement(continueStatement, scope);
                break;
            }
            case BreakStatementSyntax breakStatement:
            {
                CompileBreakStatement(breakStatement, scope);
                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported statement syntax: {statement.Kind()}");
            }
        }
    }

    private void CompileLocalVariableDeclarationStatement(LocalVariableDeclarationStatementSyntax localVariableDeclaration, Scope scope)
    {
        if (localVariableDeclaration.EqualsValues is null)
        {
            prototype.EmitInstruction(Operation.OP_LOADNIL, Register, localVariableDeclaration.Names.Count);

            foreach (var name in localVariableDeclaration.Names)
            {
                scope.VariableMapping.Add(name.Name, Register);
                AddRegister();
            }

            return;
        }

        var valueCount = localVariableDeclaration.EqualsValues.Values.Count;
        var nameCount = localVariableDeclaration.Names.Count;
        var emitLoadNil = valueCount > nameCount;

        foreach (var name in localVariableDeclaration.Names)
        {
            scope.VariableMapping[name.Name] = scope.VariableMapping.Count == 0 ? 0 : scope.VariableMapping.MaxBy(x => x.Value).Value+1;
        }

        for (var index = 0; index < localVariableDeclaration.Names.Count; index++)
        {
            
            if (valueCount > index)
            {
                CompileExpression(localVariableDeclaration.EqualsValues.Values[index], scope);    
            }
            
            //=AddRegister();
        }

        if (emitLoadNil)
        {
            prototype.EmitInstruction(Operation.OP_LOADNIL, nameCount, valueCount); // This is wrong needs fix
        }
    }

    private void CompileAssignmentStatement(AssignmentStatementSyntax assignment, Scope scope)
    {
        var sourceRegister = Register; // i needed this for something and I forgot why, oops
        
        var valueCount = assignment.EqualsValues.Values.Count;
        var variableCount = assignment.Variables.Count;
        
        for (var index = 0; index < variableCount; index++)
        {
            var variable = assignment.Variables[index];
            
            if (variableCount + 1 == index)
            {
                CompileExpressionWithRegister(variable, scope);
            }
            else
            {
                var value = assignment.EqualsValues.Values[index];
                
                switch (variable)
                {
                    case IdentifierNameSyntax identifierName:
                    {
                        Register--;
                        CompileExpressionWithRegister(value, scope);
                        break;
                    }
                    default:
                    {
                        CompileExpressionWithRegister(variable, scope);
                        CompileExpressionWithRegister(value, scope);
                        break;
                    }
                }
            }
        }

        if (variableCount > valueCount)
        {
            prototype.EmitInstruction(Operation.OP_LOADNIL, Register+variableCount, Register + valueCount);
        }
    }

    private void CompileCompoundAssignmentStatement(CompoundAssignmentStatementSyntax compoundAssignment, Scope scope)
    {
        var sourceRegister = Register;
        
        switch (compoundAssignment.Kind())
        {
            case SyntaxKind.AddAssignmentStatement:
            case SyntaxKind.SubtractAssignmentStatement:
            case SyntaxKind.MultiplyAssignmentStatement:
            case SyntaxKind.DivideAssignmentStatement:
            case SyntaxKind.ExponentiateAssignmentStatement: 
            case SyntaxKind.ModuloExpression:
            {
                var operation = GetArithmeticOperation(SyntaxFacts.GetCompoundAssignmentOperator(compoundAssignment.AssignmentOperatorToken.Kind()).Value);
                
                var variable = CompilePrefixExpression(compoundAssignment.Variable, scope);

                Register -= 1;
                
                var assignment = CompileExpressionWithRegister(compoundAssignment.Expression, scope);
                
                prototype.EmitInstruction(operation, Register, variable, assignment);
                
                break;
            }
            case SyntaxKind.ConcatAssignmentStatement:
            {
                var variable = CompilePrefixExpression(compoundAssignment.Variable, scope);
                CompileExpression(compoundAssignment.Expression, scope);

                prototype.EmitInstruction(Operation.OP_CONCAT, variable, Register, Register);
                
                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported compound assignment expression syntax: {compoundAssignment.Kind()}");
            }
        }
    }

    private void CompileReturnStatement(ReturnStatementSyntax @return, Scope scope) {
        foreach (var returnExpression in @return.Expressions) {
            CompileExpression(returnExpression, scope);
        }

        prototype.EmitInstruction(Operation.OP_RETURN, 0, @return.Expressions.Count + 1);
    }
    
    private void CompileDoStatement(DoStatementSyntax @do, Scope scope)
    {
        var doScope = new Scope(this);
        
        foreach (var statement in @do.Body.Statements)
        {
            CompileStatement(statement, doScope);
        }

        Register = doScope.OldTop;
    }
    
    private void CompileIfStatement(IfStatementSyntax @if, Scope scope)
    {
        var doScope = new Scope(this);

        var previousInstructions = prototype.Instructions.Count;

        switch (@if.Condition)
        {
            case BinaryExpressionSyntax binaryExpressionSyntax:
            {
                CompileBinaryExpression(binaryExpressionSyntax, scope);
                break;
            }
            case LiteralExpressionSyntax literalExpressionSyntax:
            {
                CompileLiteralExpression(literalExpressionSyntax, scope);
                break;
            }
        }

        int expressions = (prototype.Instructions.Count-2 - previousInstructions) / 2;

        previousInstructions = prototype.Instructions.Count;

        foreach (var statement in @if.Body.Statements)
        {
            CompileStatement(statement, doScope);
        }

        var blockSize = prototype.Instructions.Count - previousInstructions;

        for (int i = 0; i < expressions * 2; i += 2)
        {
            switch (@if.Condition.Kind()) // patch JUMP(s)
            {
                case SyntaxKind.LogicalAndExpression:
                {
                    prototype.Instructions[^(blockSize + 3 + i)].A = blockSize + 2 + i;
                    prototype.Instructions[^(blockSize + 1 + i)].A = blockSize + i;
                    break;
                }
                case SyntaxKind.LogicalOrExpression:
                {

                    prototype.Instructions[^(blockSize + 3 + i)].A = 2 + i;
                    prototype.Instructions[^(blockSize + 1 + i)].A = blockSize + i;
                    break;
                }
                default:
                {
                    prototype.Instructions[^(blockSize + 1 + i)].A = blockSize + i;
                    break;
                }
            }
        }

        Register = doScope.OldTop;
    }

    private void CompileNumericForStatement(NumericForStatementSyntax numericFor, Scope scope)
    {
        // Needs scopes
    }

    private void CompileGenericForStatement(GenericForStatementSyntax genericFor, Scope scope)
    {
        // Needs scopes
    }

    private void CompileLocalFunctionDeclarationStatement(LocalFunctionDeclarationStatementSyntax localFunctionDeclaration, Scope scope)
    {
        // Needs scopes
    }

    private void CompileFunctionDeclarationStatement(FunctionDeclarationStatementSyntax functionDeclaration, Scope scope)
    {
        // Needs scopes
    }

    private void CompileWhileStatement(WhileStatementSyntax @while, Scope scope)
    {
        // Needs scopes
    } 
    
    private void CompileRepeatUntilStatement(RepeatUntilStatementSyntax repeatUntil, Scope scope)
    {
        // Needs scopes
    }

    private void CompileContinueStatement(ContinueStatementSyntax @continue, Scope scope)
    {
        // Needs scopes
    }

    private void CompileBreakStatement(BreakStatementSyntax @break, Scope scope)
    {
        // Needs scopes
    }

    private void CompileExpression(ExpressionSyntax expression, Scope scope)
    {
        switch (expression)
        {
            case LiteralExpressionSyntax literalExpression: 
            {
                CompileLiteralExpression(literalExpression, scope);
                break;
            }
            case PrefixExpressionSyntax prefixExpression:
            {
                CompilePrefixExpression(prefixExpression, scope);
                break;
            }
            case UnaryExpressionSyntax unaryExpression:
            {
                CompileUnaryExpression(unaryExpression, scope);
                break;
            }
            case BinaryExpressionSyntax binaryExpression:
            {
                CompileBinaryExpression(binaryExpression, scope);
                break;
            }
            case VarArgExpressionSyntax varargExpression:
            {
                CompileVarArgExpression(varargExpression, scope);
                break;
            }
            case TableConstructorExpressionSyntax tableConstructorExpression:
            {
                CompileTableConstructorExpression(tableConstructorExpression, scope);
                break;
            }
            case AnonymousFunctionExpressionSyntax anonymousFunctionExpression:
            {
                CompileAnonymousFunctionExpression(anonymousFunctionExpression, scope);
                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported expression syntax: {expression.Kind()}");
            }
        }
    }
    
    private int CompilePrefixExpression(PrefixExpressionSyntax prefix, Scope scope)
    {
        switch (prefix)
        {
            case IdentifierNameSyntax identifierName:
            {
                CompileIdentifierName(identifierName, scope);
                break;
            }
            case ParenthesizedExpressionSyntax parenthesizedExpression: 
            {
                CompileExpression(parenthesizedExpression.Expression, scope);
                break;
            }
            case FunctionCallExpressionSyntax functionCallExpression: 
            {
                CompileFunctionCallExpression(functionCallExpression, scope);
                break;
            }
            case MethodCallExpressionSyntax methodCallExpression:
            {
                CompileMethodCallExpression(methodCallExpression, scope);
                break;
            }
            case MemberAccessExpressionSyntax memberAccessExpression:
            {
                CompileMemberAccessExpression(memberAccessExpression, scope);
                break;
            }
            case ElementAccessExpressionSyntax elementAccessExpression:
            {
                CompileElementAccessExpression(elementAccessExpression, scope);
                break;
            }
        }

        return Register;
    }
    
    private void CompileLiteralExpression(LiteralExpressionSyntax literal, Scope scope)
    {
        if (literal.Parent is not EqualsValuesClauseSyntax && literal.Parent?.Parent is not FunctionCallExpressionSyntax)
            return;

        switch (literal.Kind())
        {
            case SyntaxKind.StringLiteralExpression:
            {
                prototype.EmitInstruction(Operation.OP_LOADK, Register, module.EmitK((string)literal.Token.Value!));
                break;
            }
            case SyntaxKind.NumericalLiteralExpression when literal.Token.Value is double d:
            {
                prototype.EmitInstruction(Operation.OP_LOADK, Register, module.EmitK(d));
                break;
            }
            case SyntaxKind.TrueLiteralExpression: 
            {
                prototype.EmitInstruction(Operation.OP_LOADK, Register, module.EmitK(true));
                break;
            }
            case SyntaxKind.FalseLiteralExpression: 
            {
                prototype.EmitInstruction(Operation.OP_LOADK, Register, module.EmitK(false));
                break;
            }
            case SyntaxKind.NilLiteralExpression:
            {
                prototype.EmitInstruction(Operation.OP_LOADNIL, Register, Register, module.EmitK(null!));
                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported literal expression syntax: {literal.Kind()}");
            }
        }
        
        AddRegister();
    }

    private void CompileUnaryExpression(UnaryExpressionSyntax unary, Scope scope)
    {
        CompileExpression(unary.Operand, scope);
        
        switch (unary.Kind())
        {
            case SyntaxKind.UnaryMinusExpression:
            {
                prototype.EmitInstruction(Operation.OP_UNM, Register, Register-1);
                break;
            }
            case SyntaxKind.LengthExpression: 
            {
                prototype.EmitInstruction(Operation.OP_LEN, Register, Register-1);
                break;
            }
            case SyntaxKind.LogicalNotExpression: 
            {
                prototype.EmitInstruction(Operation.OP_NOT, Register, Register-1);
                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported unary expression syntax: {unary.Kind()}");
            }
        } 
        
        AddRegister();
    }

    private void CompileBinaryExpression(BinaryExpressionSyntax binary, Scope scope)
    {
        int CompileExpr(ExpressionSyntax syntax) => syntax switch
        {
            LiteralExpressionSyntax literalExpression => module.EmitK(literalExpression.Token.Value!),
            IdentifierNameSyntax identifierName => scope.VariableMapping[identifierName.Name],
            _ => throw new Exception("shouldnt have reached here")
        };

        var startRegister = Register;

        var leftExpression = binary.Left;
        var rightExpression = binary.Right;

        switch (binary.Kind())
        {
            case SyntaxKind.AddExpression:
            case SyntaxKind.SubtractExpression:
            case SyntaxKind.MultiplyExpression:
            case SyntaxKind.DivideExpression:
            case SyntaxKind.ModuloExpression:
            case SyntaxKind.ExponentiateExpression:
            {
                var leftPos = CompileExpressionWithRegister(leftExpression, scope);
                AddRegister();
                var rightPos = CompileExpressionWithRegister(rightExpression, scope);
                AddRegister();

                Register -= 2;
                prototype.EmitInstruction(GetArithmeticOperation(binary.OperatorToken.Kind()), Register, leftPos, rightPos);
                break;
            }
            case SyntaxKind.ConcatExpression:
            {
                var concatExpressions = new List<ExpressionSyntax>();

                // unroll the tree of concats down the right hand side to be able to do multiple ops
                var currentExpression = (ExpressionSyntax)binary;
                while (currentExpression is BinaryExpressionSyntax binOp)
                {
                    if (!binOp.Left.IsKind(SyntaxKind.ConcatExpression))
                    {
                        concatExpressions.Add(binOp.Left);
                    }
                    else
                    {
                        currentExpression = binOp.Left;
                    }
                    
                    concatExpressions.Add(binOp.Right);
                }

                var sourceRegister = Register;

                foreach (var expression in concatExpressions)
                {
                    MoveIfFalse(Register, CompileExpressionWithRegister(expression, scope), true);
                }
                
                Register = startRegister;
                prototype.EmitInstruction(Operation.OP_CONCAT, sourceRegister, Register, Register + concatExpressions.Count);
                
                break;
            }
            case SyntaxKind.EqualsExpression:
            case SyntaxKind.NotEqualsExpression:
            case SyntaxKind.LessThanExpression:
            case SyntaxKind.LessThanOrEqualExpression:
            case SyntaxKind.GreaterThanExpression:
            case SyntaxKind.GreaterThanOrEqualExpression:
            {
                prototype.EmitInstruction(Operation.OP_EQ, 0, CompileExpr(binary.Left), CompileExpr(binary.Right));
                prototype.EmitInstruction(Operation.OP_JMP); // we will patch this later
                break;
            } 
            case SyntaxKind.LogicalOrExpression:
            case SyntaxKind.LogicalAndExpression:
            {
                var value = binary;
                while (value.Kind() is SyntaxKind.LogicalAndExpression or SyntaxKind.LogicalOrExpression)
                {
                    /*if (binary.Left is BinaryExpressionSyntax syntax1)
                    {
                        prototype.EmitInstruction(Operation.EQ, binary.Kind() == SyntaxKind.LogicalOrExpression ? 1 : 0, CompileExpr(syntax1.Left), CompileExpr(syntax1.Right));
                        prototype.EmitInstruction(Operation.JUMP, int.MinValue + 1); // we will patch this later
                    }*/

                    if (binary.Right is BinaryExpressionSyntax syntax2)
                    {
                        prototype.EmitInstruction(Operation.OP_EQ, 0, CompileExpr(syntax2.Left), CompileExpr(syntax2.Right));
                        prototype.EmitInstruction(Operation.OP_JMP, 0); // we will patch this later
                    }

                    if (value.Left is BinaryExpressionSyntax syntax3)
                        value = syntax3;
                }

                prototype.EmitInstruction(Operation.OP_EQ, binary.Kind() == SyntaxKind.LogicalOrExpression ? 1 : 0, CompileExpr(value.Left), CompileExpr(value.Right));
                prototype.EmitInstruction(Operation.OP_JMP); // we will patch this later


                break;
            }
            default:
            {
                throw new UnsupportedSyntaxException($"Unsupported binary expression syntax: {binary.Kind()}");
            }
        }
    }
    
    private void CompileVarArgExpression(VarArgExpressionSyntax varargExpression, Scope scope)
    {
        prototype.HasVararg = true;
        prototype.EmitInstruction(Operation.OP_VARARG, Register);
    }

    private void CompileTableConstructorExpression(TableConstructorExpressionSyntax tableConstructorExpression, Scope scope)
    {
        var tableLocation = Register;
        
        prototype.EmitInstruction(Operation.OP_NEWTABLE, Register);
        
        AddRegister();
        
        
        if (tableConstructorExpression.Fields.Count == 0)
        {
            return;    
        }

        var arraySize = 0;

        foreach (var field in tableConstructorExpression.Fields)
        {
           switch (field)
           {
               case UnkeyedTableFieldSyntax unkeyedTableField:
               {
                   arraySize++;
                   
                   var softValue = CompileExpressionWithRegister(unkeyedTableField.Value, scope);

                   prototype.EmitInstruction(Operation.OP_SETTABLE, tableLocation, softValue, module.EmitK(arraySize));

                   break;
               }
               case IdentifierKeyedTableFieldSyntax identifierKeyedTableField:
               {
                   var softValue = CompileExpressionWithRegister(identifierKeyedTableField.Value, scope);
                
                   prototype.EmitInstruction(Operation.OP_SETTABLE, tableLocation, softValue, module.EmitK(identifierKeyedTableField.Identifier.Text));
                   
                   break;
               }
               case ExpressionKeyedTableFieldSyntax expressionKeyedTableField:
               {
                   var key = CompileExpressionWithRegister(expressionKeyedTableField.Key, scope);
                   var softValue = CompileExpressionWithRegister(expressionKeyedTableField.Value, scope);

                   prototype.EmitInstruction(Operation.OP_SETTABLE, tableLocation, key, softValue);
                   
                   break;
               }
            }
        }
    }
    
    private void CompileIdentifierName(IdentifierNameSyntax identifierName, Scope scope)
    {
        var name = identifierName.Name;
        
        if (scope.VariableMapping.TryGetValue(name, out var sourceRegister))
        {
            MoveIfFalse(Register, sourceRegister, false);
            
            return;
        }

        if (AvailableHeapValues.TryGetValue(name, out var heapIndex))
        {
            prototype.EmitInstruction(Operation.OP_GETUPVAL, Register, heapIndex);
            AddRegister();
            
            return;
        }

        prototype.EmitInstruction(Operation.OP_GETGLOBAL, Register, module.EmitK(name));

        //AddRegister();
    }

    private void CompileFunctionCallExpression(FunctionCallExpressionSyntax functionCall, Scope scope)
    {
        var functionLocation = CompilePrefixExpression(functionCall.Expression, scope);
        var arguments = GetFunctionArguments(functionCall.Argument);
        var results = 0;

        var sourceRegister = Register;
        var argumentCount = arguments.Count;
        
        AddRegister();

        if (Register - 1 != functionLocation)
        {
            MoveIfFalse(Register - 1, functionLocation);
            functionLocation = Register - 1;
        }

        foreach (var argument in arguments)
        {
            var softValue = CompileExpressionWithRegister(argument, scope);
            
            if (softValue != Register)
            {
                MoveIfFalse(Register, softValue);
            }
            
            AddRegister();
        }

        var lastInstruction = prototype.Instructions.Last();
        if (lastInstruction.Operation == Operation.OP_VARARG)
        {
            argumentCount += lastInstruction.B - 1;
        }

        if (functionCall.Parent?.Parent is LocalVariableDeclarationStatementSyntax syntax)  
        {
            results = syntax.Names.Count;
        }

        prototype.EmitInstruction(Operation.OP_CALL, functionLocation, argumentCount+1, results+1);
        
        Register = sourceRegister+results;
    }
    
    private void CompileMethodCallExpression(MethodCallExpressionSyntax methodCall, Scope scope)
    {
        var tableLocation = CompilePrefixExpression(methodCall.Expression, scope);
        
        prototype.EmitInstruction(Operation.OP_SELF, Register, tableLocation, module.EmitK(methodCall.Identifier.Text));
    }

    private void CompileMemberAccessExpression(MemberAccessExpressionSyntax memberAccess, Scope scope)
    {
        var softValue = CompileExpressionWithRegister(memberAccess.Expression, scope);
        
        prototype.EmitInstruction(Operation.OP_GETTABLE, Register, softValue, module.EmitK(memberAccess.MemberName.Text));
    }
    
    private void CompileElementAccessExpression(ElementAccessExpressionSyntax elementAccess, Scope scope)
    {
        var softValue = CompileExpressionWithRegister(elementAccess.Expression, scope);
        
        if (elementAccess.KeyExpression is LiteralExpressionSyntax literalExpression)
        {
            switch (literalExpression.Token.Value)
            {
                case string s:
                {
                    prototype.EmitInstruction(Operation.OP_GETTABLE, Register, softValue, module.EmitK(s));
                    break;
                }
                case double d:
                {
                    prototype.EmitInstruction(Operation.OP_GETTABLE, Register, softValue, module.EmitK(d));
                    break;
                }
                default:
                {
                    goto notConstant;
                }
            }
            
            return;
        }

        notConstant:;

        var sourceRegister = Register;

        var keySoftValue = CompileExpressionWithRegister(elementAccess.KeyExpression, scope);

        Register = sourceRegister;

        prototype.EmitInstruction(Operation.OP_GETTABLE, Register, softValue, keySoftValue);
    }

    private void CompileAnonymousFunctionExpression(AnonymousFunctionExpressionSyntax anonymousFunction, Scope scope)
    {
        // Needs scopes
    }

    // ReSharper disable twice InconsistentNaming
    private int Top = 0;
    private int Register = 0;
    
    private readonly Dictionary<string, int> AvailableHeapValues;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void AddRegister(int increase = 1)
    {
        Register += increase;
        
        if (Register > Top)
        {
            Top = Register;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void MoveIfFalse(int newRegister, int source, bool increase = false)
    {
        if (newRegister != source)
        {
            prototype.EmitInstruction(Operation.OP_MOVE, newRegister, source);
        }

        if (increase)
        {
            AddRegister();
        }
    }
    
    private int CompileExpressionWithRegister(ExpressionSyntax expression, Scope scope)
    {
        CompileExpression(expression, scope);
        return Register;
    }

    private void PatchJump(int start, int end)
    {
        var startInstruction = prototype.Instructions[start];

        var operation = startInstruction.Operation;

        if (operation is Operation.OP_JMP or Operation.OP_TEST or Operation.OP_FORLOOP or Operation.OP_FORPREP)
        {
            startInstruction.B = end - start;
        }
        else
        {
            throw new InternalException($"Unexpected operation passed to PatchJump: {operation}");
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private Operation GetArithmeticOperation(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.PlusToken => Operation.OP_ADD,
            SyntaxKind.MinusToken => Operation.OP_SUB,
            SyntaxKind.StarToken => Operation.OP_MUL,
            SyntaxKind.SlashToken => Operation.OP_DIV,
            SyntaxKind.HatToken => Operation.OP_POW,
            SyntaxKind.PercentToken => Operation.OP_MOD,
            
            _ => throw new InternalException($"Unexpected syntax kind passed to GetArithmeticOperation: {kind}")
        };
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private Operation GetEqualsOperation(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.EqualsEqualsToken => Operation.OP_EQ,
            SyntaxKind.LessThanEqualsToken => Operation.OP_LE,
            SyntaxKind.LessThanToken => Operation.OP_LT,
            SyntaxKind.GreaterThanEqualsToken => Operation.OP_LE,
            SyntaxKind.GreaterThanToken => Operation.OP_LT,

            _ => throw new InternalException($"Unexpected syntax kind passed to GetArithmeticOperation: {kind}")
        };
    }

    private ImmutableList<ExpressionSyntax> GetFunctionArguments(FunctionArgumentSyntax functionArgument)
    {
        return functionArgument switch
        {
            StringFunctionArgumentSyntax stringFunctionArgument => ImmutableList.Create<ExpressionSyntax>(stringFunctionArgument.Expression),
            TableConstructorFunctionArgumentSyntax tableConstructorFunctionArgument => ImmutableList.Create<ExpressionSyntax>(tableConstructorFunctionArgument.TableConstructor),
            ExpressionListFunctionArgumentSyntax expressionListFunctionArgument => expressionListFunctionArgument.Expressions.ToImmutableList(),
            _ => throw new InternalException($"Incorrect kind passed to GetFunctionArguments: {functionArgument.Kind()}")
        };
    }
}