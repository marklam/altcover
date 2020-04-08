namespace AltCover.Commands

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Management.Automation
open System.Xml.Linq
open System.Xml.XPath

#if TODO
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.PowerShell",
                                                  "PS1101:AllCmdletsShouldAcceptPipelineInput",
                                                  Justification = "TODO")>]
[<Cmdlet(VerbsCommon.Select, "ByTracking")>]
[<OutputType(typeof<XmlDocument>)>]
type SelectByTrackingCommand(outputFile : String) =
  inherit PSCmdlet()

  new() = SelectByTrackingCommand(String.Empty)

  [<Parameter(Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set
#endif

[<Cmdlet(VerbsData.Merge, "Coverage")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
[<SuppressMessage("Microsoft.PowerShell", "PS1003:DoNotAccessPipelineParametersOutsideProcessRecord",
  Justification="The rule gets confused by EndProcessing calling whileInCurrentDirectory")>]
[<SuppressMessage("Microsoft.PowerShell", "PS1003:DoNotAccessPipelineParametersOutsideProcessRecord",
  Justification="The rule gets confused by EndProcessing calling whileInCurrentDirectory")>]
type MergeCoverageCommand() =
  inherit PSCmdlet()

  let whileInCurrentDirectory (self:PSCmdlet) f =
    let here = Directory.GetCurrentDirectory()
    try
       let where = self.SessionState.Path.CurrentLocation.Path
       Directory.SetCurrentDirectory where
       f()
    finally
      Directory.SetCurrentDirectory here

  [<Parameter(ParameterSetName = "XmlDoc", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<ValidateNotNull; ValidateCount(1, Int32.MaxValue)>]
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  [<SuppressMessage("Microsoft.Performance", "CA1819",
                    Justification = "ditto, ditto")>]
  member val XDocument : XDocument array = [||] with get, set

  [<Parameter(ParameterSetName = "Files", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<ValidateNotNull; ValidateCount(1, Int32.MaxValue)>]
  [<SuppressMessage(
      "Gendarme.Rules.Performance", "AvoidReturningArraysOnPropertiesRule",
      Justification = "Cannot convert 'System.Object[]' to the type 'System.Collections.Generic.IEnumerable`1[System.String]'")>]
  [<SuppressMessage("Microsoft.Performance", "CA1819",
                    Justification = "ditto, ditto")>]
  member val InputFile : string array = [||] with get, set

  [<Parameter(Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = String.Empty with get, set

  [<Parameter(Mandatory = false)>]
  member val AsNCover : SwitchParameter = SwitchParameter(false) with get, set

  member val private Files = new List<XDocument>()

  override self.BeginProcessing() = self.Files.Clear()

  [<SuppressMessage("Microsoft.Usage", "CA2208:InstantiateArgumentExceptionsCorrectly",
    Justification="Inlined library code")>]
  member private self.FilesToDocuments() =
    self.InputFile
    |> Array.map XDocument.Load
    |> self.Files.AddRange

  override self.ProcessRecord() =
    whileInCurrentDirectory self (fun _ ->
      if self.ParameterSetName.StartsWith("File", StringComparison.Ordinal)
      then self.FilesToDocuments()
      self.Files.AddRange self.XDocument)

  override self.EndProcessing() =
    whileInCurrentDirectory self (fun _ ->
      let xmlDocument =
        AltCover.OpenCoverUtilities.MergeCoverage self.Files self.AsNCover.IsPresent
      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument)

[<Cmdlet(VerbsData.Compress, "Branching")>]
[<OutputType(typeof<XDocument>); AutoSerializable(false)>]
type CompressBranchingCommand(outputFile : String) =
  inherit PSCmdlet()

  new() = CompressBranchingCommand(String.Empty)

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val XDocument : XDocument = null with get, set

  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 1,
              ValueFromPipeline = true, ValueFromPipelineByPropertyName = false)>]
  member val InputFile : string = null with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = false, Position = 3,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputFile : string = outputFile with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val SameSpan : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "XmlDocA", Mandatory = false, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileA", Mandatory = false, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "XmlDocB", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "FromFileB", Mandatory = true, Position = 4,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val WithinSequencePoint : SwitchParameter = SwitchParameter(false) with get, set

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      if self.ParameterSetName.StartsWith("FromFile", StringComparison.Ordinal) then
        self.XDocument <- XDocument.Load self.InputFile

      let xmlDocument =
        AltCover.OpenCoverUtilities.CompressBranching self.XDocument
          self.WithinSequencePoint.IsPresent self.SameSpan.IsPresent

      if self.OutputFile
         |> String.IsNullOrWhiteSpace
         |> not
      then xmlDocument.Save(self.OutputFile)

      self.WriteObject xmlDocument
    finally
      Directory.SetCurrentDirectory here