// See https://aka.ms/new-console-template for more information

using System.Security.Cryptography;
using Compiler.Core;

 Compiler.Core.Compiler.Compile(@"
table.pack(1,2,(function() return 3 end)())
");