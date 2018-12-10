namespace AltCover.Commands

open System
open System.IO
open System.Management.Automation
open AltCover

[<Cmdlet(VerbsLifecycle.Invoke, "AltCover")>]
[<OutputType("System.Void")>]
[<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.PowerShell",
                                                  "PS1101:AllCmdletsShouldAcceptPipelineInput",
                                                  Justification = "No valid input")>]
type InvokeAltCoverCommand(runner : bool) =
  inherit PSCmdlet()
  new() = InvokeAltCoverCommand(false)

  [<Parameter(ParameterSetName = "Runner", Mandatory = true, Position = 1,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Runner : SwitchParameter = SwitchParameter(runner) with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = true, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val RecorderDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val WorkingDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Executable = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val LcovReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Threshold = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val Cobertura = String.Empty with get, set

  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val OutputFile = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Parameter(ParameterSetName = "Runner", Mandatory = false, ValueFromPipeline = false,
              ValueFromPipelineByPropertyName = false)>]
  member val CommandLine : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InputDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OutputDirectory = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("SymbolDirectories")>]
  member val SymbolDirectory : string array = [||] with get, set
#if NETCOREAPP2_0

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("Dependencies")>]
  member val Dependency : string array = [||] with get, set
#else
  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  [<Alias("Keys")>]
  member val Key  : string array = [| |] with get, set
  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
      ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val StrongNameKey = String.Empty with get, set
#endif

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val XmlReport = String.Empty with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val FileFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val PathFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AssemblyExcludeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val TypeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val MethodFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val AttributeFilter : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val CallContext : string array = [||] with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val OpenCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val InPlace : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Save : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Single : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val LineCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Instrument", Mandatory = false,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val BranchCover : SwitchParameter = SwitchParameter(false) with get, set

  [<Parameter(ParameterSetName = "Version", Mandatory = true, Position = 1,
              ValueFromPipeline = false, ValueFromPipelineByPropertyName = false)>]
  member val Version : SwitchParameter = SwitchParameter(false) with get, set

  member val private Fail : String list = [] with get, set

  member private self.Collect() =
    { CollectParameters.Create() with RecorderDirectory = DirectoryPath self.RecorderDirectory
                                      WorkingDirectory = DirectoryPath self.WorkingDirectory
                                      Executable = FilePath self.Executable
                                      LcovReport = FilePath self.LcovReport
                                      Threshold = match Byte.TryParse self.Threshold with
                                                  | (true, n) -> Threshold n
                                                  | _ -> NoThreshold
                                      Cobertura = FilePath self.Cobertura
                                      OutputFile = FilePath self.OutputFile
                                      CommandLine = self.CommandLine
                                                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                                                    |> Seq.map CommandArgument |> Command }

  member private self.Prepare() =
    { PrepareParameters.Create() with InputDirectory = DirectoryPath self.InputDirectory
                                      OutputDirectory = DirectoryPath self.OutputDirectory
                                      SymbolDirectories = self.SymbolDirectory
                                                          |>Seq.map DirectoryPath
                                                          |> DirectoryPaths
#if NETCOREAPP2_0
                                      Dependencies = self.Dependencies
                                                     |> Seq.map FilePath
                                                     |> FilePaths
#else
                                      Keys = self.Key
                                             |> Seq.map FilePath
                                             |> FilePaths
                                      StrongNameKey = FilePath self.StrongNameKey
#endif
                                      XmlReport = FilePath self.XmlReport
                                      FileFilter = self.FileFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      AssemblyFilter = self.AssemblyFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      AssemblyExcludeFilter = self.AssemblyExcludeFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      TypeFilter = self.TypeFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      MethodFilter = self.MethodFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      AttributeFilter = self.AttributeFilter
                                                        |> Seq.map Raw
                                                        |> Filters
                                      PathFilter = self.PathFilter
                                                   |> Seq.map Raw
                                                   |> Filters
                                      CallContext = self.CallContext
                                                    |> Seq.map (fun c -> match Byte.TryParse c with
                                                                         | (true, n) -> TimeItem n
                                                                         | _ -> CallItem c)
                                                    |> Context
                                      OpenCover = Flag self.OpenCover.IsPresent
                                      InPlace = Flag self.InPlace.IsPresent
                                      Save = Flag self.Save.IsPresent
                                      Single = Flag self.Single.IsPresent
                                      LineCover = Flag self.LineCover.IsPresent
                                      BranchCover = Flag self.BranchCover.IsPresent
                                      CommandLine = self.CommandLine
                                                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                                                    |> Seq.map CommandArgument |> Command }

  member private self.Log() =
    { Logging.Default with Error = (fun s -> self.Fail <- s :: self.Fail)
                           Info = (fun s -> self.WriteInformation(s, [||]))
                           Warn = (fun s -> self.WriteWarning s) }

  override self.ProcessRecord() =
    let here = Directory.GetCurrentDirectory()
    let log = self.Log()
    try
      let where = self.SessionState.Path.CurrentLocation.Path
      Directory.SetCurrentDirectory where
      let makeError s =
        ErrorRecord(InvalidOperationException(), s, ErrorCategory.InvalidOperation, self)
        |> self.WriteError

      let status =
        (match (self.Version.IsPresent, self.Runner.IsPresent) with
         | (true, _) ->
           (fun _ ->
           Api.Version() |> log.Info
           0)
         | (_, true) ->
           let task = self.Collect()
           Api.DoCollect task
         | _ ->
           let task = self.Prepare()
           Api.DoPrepare task) log
      if status <> 0 then status.ToString() |> self.Log().Error
      match self.Fail with
      | [] -> ()
      | things -> String.Join(Environment.NewLine, things |> List.rev) |> makeError
    finally
      Directory.SetCurrentDirectory here