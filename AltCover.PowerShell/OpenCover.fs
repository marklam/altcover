namespace AltCover.Commands

open System
open System.Collections.Generic
open System.IO
open System.Management.Automation
open System.Xml
open System.Xml.XPath

[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.PowerShell",
  "PS1101:AllCmdletsShouldAcceptPipelineInput",
  Justification = "TODO")>]
[<Cmdlet(VerbsCommon.Select, "ByTracking")>]
[<OutputType(typeof<XmlDocument>)>]
type SelectByTrackingCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = SelectByTrackingCommand(String.Empty)

  [<Parameter(Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

[<Cmdlet(VerbsData.Merge, "Coverage")>]
[<OutputType(typeof<XmlDocument>)>]
type MergeCoverageCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = MergeCoverageCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<ValidateNotNull;ValidateCount(1,Int32.MaxValue)>]
  member val XmlDocument:IXPathNavigable array = [| |] with get, set

  [<Parameter(ParameterSetName = "Files", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<ValidateNotNull;ValidateCount(1,Int32.MaxValue)>]
  member val InputFile:string array = [| |] with get, set

  [<Parameter(Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  [<Parameter(Mandatory = false)>]
  member val AsNCover : SwitchParameter = SwitchParameter(false) with get, set

  member val private Files = new List<IXPathNavigable>() with get

  override self.BeginProcessing() =
    self.Files.Clear()

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where

      if self.ParameterSetName.StartsWith("File", StringComparison.Ordinal) then
        self.XmlDocument <- self.InputFile
                            |>  Array.map (fun x -> XPathDocument(x) :> IXPathNavigable)
      self.Files.AddRange self.XmlDocument
    finally
      Directory.SetCurrentDirectory here

  override self.EndProcessing() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where

      let xmlDocument = AltCover.OpenCoverUtilities.MergeCoverage self.Files self.AsNCover.IsPresent
      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here

[<Cmdlet(VerbsData.Compress, "Branching")>]
[<OutputType(typeof<XmlDocument>)>]
type CompressBranchingCommand(outputFile:String) =
  inherit PSCmdlet()

  new () = CompressBranchingCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XmlDocument:IXPathNavigable = null with get, set

  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 1,
      ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile:string = null with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = false, Position = 3,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile:string = outputFile with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SameSpan:SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 4,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WithinSequencePoint:SwitchParameter = SwitchParameter(false) with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName.StartsWith("FromFile", StringComparison.Ordinal) then
        self.XmlDocument <- XPathDocument self.InputFile

      let xmlDocument = AltCover.OpenCoverUtilities.CompressBranching self.XmlDocument
                            self.WithinSequencePoint.IsPresent
                            self.SameSpan.IsPresent

      if self.OutputFile |> String.IsNullOrWhiteSpace |> not then
        xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here