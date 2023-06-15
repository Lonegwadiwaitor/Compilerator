namespace Compiler.Core.Exceptions;

public class UnsupportedSyntaxException : Exception
{
    public UnsupportedSyntaxException(string message) : base(message) { }
}