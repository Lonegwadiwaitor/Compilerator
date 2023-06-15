namespace Compiler.Core.Exceptions;

public class SyntaxErrorException : Exception
{
    public SyntaxErrorException(string message) : base(message) { }
}