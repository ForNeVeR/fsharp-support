namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Stages

open JetBrains.ReSharper.Daemon.Stages
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.TypeHints
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.UI.RichText
open Microsoft.FSharp.Compiler.SourceCodeServices

type TypeHighlightingVisitor(fsFile: IFSharpFile) =
    inherit TreeNodeVisitor<ResizeArray<HighlightingInfo>>()
    
    let typeToString(fSharpType: FSharpType) = fSharpType.ToString()
    
    let toToolTipText(symbol: FSharpSymbol) =
        match symbol with
        | Symbol.MemberFunctionOrValue mem -> Some(typeToString mem.FullType)
        | :? FSharpParameter as param -> Some(typeToString param.Type)
        | _ -> None
    
    let getToolTipText(iLet: ILet) =
        let symbolUse = fsFile.GetSymbolUse(iLet.GetTreeStartOffset().Offset)
        if (isNull (box symbolUse)) then None
        else toToolTipText(symbolUse.Symbol)
    
    override x.VisitNode(node, context) =
        for child in node.Children() do
            match child with
            | :? IFSharpTreeNode as treeNode -> treeNode.Accept(x, context)
            | _ -> ()
    
    override x.VisitLet(iLet, context) =
        getToolTipText iLet
        |> Option.iter (fun toolTipText ->
            let range = iLet.GetHighlightingRange() 
            let highlighting = TypeHintHighlighting(RichText toolTipText, range)
            let info = HighlightingInfo(range, highlighting)
            context.Add info)
        
        x.VisitNode(iLet, context)

type TypeHintsHighlightingProcess(fsFile, daemonProcess) =
    inherit FSharpDaemonStageProcessBase(fsFile, daemonProcess)
    
    member x.CollectHighlightings() =
        let collection = ResizeArray()
        let visitor = TypeHighlightingVisitor(fsFile)
        fsFile.Accept(visitor, collection)
        collection
    
    override x.Execute(commiter) =
        let highlightings = x.CollectHighlightings()
        let result = DaemonStageResult(highlightings)
        commiter.Invoke(result)

[<DaemonStage(StagesBefore = [| typeof<GlobalFileStructureCollectorStage> |])>]
type TypeHintsStage() =
    inherit FSharpDaemonStageBase()
    
    override x.IsSupported(sourceFile, daemonProcessKind) =
        base.IsSupported(sourceFile, daemonProcessKind) && daemonProcessKind = DaemonProcessKind.VISIBLE_DOCUMENT
    
    override x.CreateStageProcess(fsFile, settings, daemonProcess) =
        upcast TypeHintsHighlightingProcess(fsFile, daemonProcess)
