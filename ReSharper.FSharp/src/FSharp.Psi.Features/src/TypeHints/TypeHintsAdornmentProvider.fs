namespace rec JetBrains.ReSharper.Plugins.FSharp.Psi.Features.TypeHints

open JetBrains.DocumentModel
open JetBrains.ProjectModel
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.TextControl.DocumentMarkup
open JetBrains.UI.RichText

type TypeHintDataModel(text) =
    interface IIntraTextAdornmentDataModel with
        member x.Text = text
        member x.HasContextMenu = false
        member x.ContextMenuTitle = null
        member x.ContextMenuItems = null
        member x.HasAction = false
        member x.Execute(textControl) = ()
        member x.IsPreceding = false

[<DaemonIntraTextAdornmentProvider(typeof<TypeHintsAdornmentProvider>)>]
[<StaticSeverityHighlighting(Severity.INFO,
     HighlightingGroupIds.IntraTextAdornmentsGroup,
     AttributeId = HighlightingAttributeIds.PARAMETER_NAME_HINT_ATTRIBUTE,
     OverlapResolve = OverlapResolveKind.NONE,
     ShowToolTipInStatusBar = false)>]
type TypeHintHighlighting(text: RichText, range: DocumentRange) =
    interface IHighlighting with
        member x.ToolTip = null
        member x.ErrorStripeToolTip = null
        member x.IsValid() = not text.IsEmpty && not range.IsEmpty;
        member x.CalculateRange() = range
        
    member x.Text = text

[<SolutionComponent>]
type TypeHintsAdornmentProvider() =
    interface IHighlighterIntraTextAdornmentProvider with
        member x.CreateDataModel(highlighter) =
            match highlighter.UserData with
            | :? TypeHintHighlighting as thh -> upcast TypeHintDataModel(thh.Text)
            | _ -> null