namespace JetBrains.ReSharper.Plugins.FSharp.Daemon.Stages

open JetBrains.DocumentModel
open JetBrains.ReSharper.Plugins.FSharp
open JetBrains.ReSharper.Daemon.Stages
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Plugins.FSharp.Daemon.Cs.Stages
open JetBrains.ReSharper.Plugins.FSharp.Psi.Features.TypeHints
open JetBrains.ReSharper.Plugins.FSharp.Psi.Tree
open JetBrains.UI.RichText
open Microsoft.FSharp.Compiler.SourceCodeServices

type TypeHighlightingVisitor(document: IDocument, checkResults: FSharpCheckFileResults) =
    inherit TreeNodeVisitor<ResizeArray<HighlightingInfo>>()
    
    let toToolTipText = function
    | FSharpToolTipElement.Group(overloads) ->
        overloads
        |> List.tryHead
        |> Option.map (fun overload -> overload.MainDescription)
    | _ -> None
    
    let getToolTipText(iLet: ILet) =
        let offset = iLet.GetTreeEndOffset().Offset
        let coords = document.GetCoordsByOffset(offset)
        let toolTipAsync = checkResults.GetToolTipText(int coords.Line + 1,
                                                       int coords.Column,
                                                       document.GetLineText coords.Line,
                                                       [iLet.GetText()],
                                                       FSharpTokenTag.Identifier)
        let (FSharpToolTipText(elements)) = toolTipAsync.RunAsTask()
        
        elements
        |> Seq.tryHead
        |> Option.bind toToolTipText
    
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

type TypeHintsHighlightingProcess(fsFile, daemonProcess, document, checkResults) =
    inherit FSharpDaemonStageProcessBase(fsFile, daemonProcess)
    
    member x.CollectHighlightings() =
        let collection = ResizeArray()
        let visitor = TypeHighlightingVisitor(document, checkResults)
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
        let getProcess doc checkRes : IDaemonStageProcess =
            upcast TypeHintsHighlightingProcess(fsFile, daemonProcess, doc, checkRes)
        
        let document = 
            Option.ofObj(fsFile.GetSourceFile())
            |> Option.map (fun sf -> sf.Document)
        let checkResults =
            fsFile.GetParseAndCheckResults true
            |> Option.map (fun r -> r.CheckResults)
            
        Option.map2 getProcess document checkResults
        |> Option.toObj
