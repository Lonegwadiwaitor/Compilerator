using Compiler.Core.Exceptions;
using Loretta.CodeAnalysis.Lua;

namespace Compiler.Core;

public enum ErrorType
{
    Internal,
    Obfuscation,
    SemanticError,
    SyntaxError
}
public sealed class Compiler
{
    public static (bool Success, byte[] byteCode, ErrorType? errorType) Compile(string source)
    {
        try
        {
            var syntaxTree = LuaSyntaxTree.ParseText(source, new LuaParseOptions(LuaSyntaxOptions.Lua51));
            var module = Compilation.Compiler.Compile(syntaxTree);

            foreach (var prototype in module.SoftPrototypes)
            {
                for (var index = 0; index < prototype.Instructions.Count; index++)
                {
                    var instruction = prototype.Instructions[index];
                    Console.WriteLine("[{0}] {1} {2} {3} {4}", index+1, instruction.Operation, instruction.A, instruction.B,
                        instruction.C);
                }
            }

            return (true, new byte[] { }, null);
        }
        catch (CompilationException)
        {
            return (false, new byte[]{}, ErrorType.Obfuscation);
        }
        catch (SemanticErrorException)
        {
            return (false, new byte[] { }, ErrorType.SemanticError);
        }
        catch (SyntaxErrorException)
        {
            return (false, new byte[] { }, ErrorType.SyntaxError);
        }
        catch (UnsupportedSyntaxException unsupportedSyntaxException)
        {
            return (false, new byte[] { }, ErrorType.SyntaxError);
        }
        catch (InternalException internalException)
        {
            return (false, new byte[] { }, ErrorType.Internal);
        }
    }
}