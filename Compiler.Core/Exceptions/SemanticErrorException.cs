namespace Compiler.Core.Exceptions;

public class SemanticErrorException : Exception
{
    public SemanticErrorException(string message) : base(message) { }
}