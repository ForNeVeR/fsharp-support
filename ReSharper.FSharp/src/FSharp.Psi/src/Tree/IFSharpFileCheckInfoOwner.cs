﻿using System;
using JetBrains.Annotations;
using JetBrains.ReSharper.Plugins.FSharp.Common.Checker;
using JetBrains.ReSharper.Psi.ExtensionsAPI.Tree;
using JetBrains.ReSharper.Psi.Parsing;
using Microsoft.FSharp.Compiler.SourceCodeServices;
using Microsoft.FSharp.Core;

namespace JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
{
  public interface IFSharpFileCheckInfoOwner : ICompositeElement
  {
    [CanBeNull]
    FSharpOption<FSharpParseAndCheckResults> GetParseAndCheckResults(bool allowStaleResults,
      bool useCachedScriptOptions, Action interruptChecker = null);

    /// <summary>
    /// True when SetResolvedSymbolsStageProcess is finished.
    /// </summary>
    bool ReferencesResolved { get; set; }

    FSharpCheckerService CheckerService { get; set; }
    FSharpProjectOptions ProjectOptions { get; set; }

    TokenBuffer ActualTokenBuffer { get; set; }
    
    [CanBeNull]
    FSharpOption<FSharpParseFileResults> ParseResults { get; set; }

    [CanBeNull]
    FSharpSymbol GetSymbolDeclaration(int offset);
    
  }
}