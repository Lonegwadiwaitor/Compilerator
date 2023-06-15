using Compiler.Core.Compilation.IR.Prototypes;

namespace Compiler.Core.Compilation.IR
{
    internal sealed class Module
    {
        public int HeapIndex = 0;

        public int EmitK(object obj)
        {
            obj ??= int.MinValue;

            if (Constants.TryGetValue(obj, out var k))
                return k;

            Constants.Add(obj, Constants.Count == 0 ? -1 : Constants.MinBy(x => x.Value).Value-1);

            return Constants[obj];
        }

        public Dictionary<object, int> Constants = new();

        public List<Prototype> SoftPrototypes = new();
    }
}
