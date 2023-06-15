namespace Compiler.Core.Exceptions;

public class CompilationException : Exception
{
    public CompilationException(string message) : base(message) { }
}