using Compiler.Core.Compilation.IR.Instructions;

namespace Compiler.Core.Compilation.IR.Prototypes;

internal class Prototype
{
    public bool HasVararg = false;
    
    public List<Instruction> Instructions = new();

    public Instruction EmitInstruction(Operation operation, int a = 0, int b = 0, int c = 0, int displacement = -1)
    {
        var newInstruction = new Instruction
        {
            Prototype = this,
            Operation = operation,
            A = a,
            B = b,
            C = c
        };

        if (displacement != -1)
        {
            Instructions.Insert(displacement, newInstruction);
            return newInstruction;
        }

        Instructions.Add(newInstruction);

        return newInstruction;
    }

    public int EmitLabel() => Instructions.Count;
}