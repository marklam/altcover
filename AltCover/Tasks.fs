namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open Microsoft.Build.Utilities
open Microsoft.Build.Framework

[<ExcludeFromCodeCoverage; NoComparison; NoEquality>]
type Logging =
  { Info : String -> unit
    Warn : String -> unit
    Error : String -> unit
    Echo : String -> unit }

  static member Default : Logging =
    { Info = ignore
      Warn = ignore
      Error = ignore
      Echo = ignore }

  static member ActionAdapter(a : Action<String>) =
    match a with
    | null -> ignore
    | _ -> a.Invoke

  member internal self.Apply() =
    Output.Error <- self.Error
    Output.Warn <- self.Warn
    Output.Info <- self.Info
    Output.Echo <- self.Echo

#nowarn "44"

module Api =
  let DoPrepare (args : PrepareParameters) (log : Logging) =
    log.Apply()
    Args.Prepare { args with
#if NETCOREAPP2_0
                                            Keys = NoPaths
                                            StrongNameKey = NoFile
#else
                                            Dependencies = NoPaths
#endif
    }
    |> List.toArray
    |> Main.EffectiveMain

  let Prepare (args : PrepareParams) (log : Logging) =
    DoPrepare (args.ToParameters()) log

  let DoCollect (args : CollectParameters) (log : Logging) =
    log.Apply()
    args
    |> Args.Collect
    |> List.toArray
    |> Main.EffectiveMain

  let Collect (args : CollectParams) (log : Logging) =
    DoCollect (args.ToParameters()) log

  let mutable internal store = String.Empty
  let private writeToStore s = store <- s
  let internal LogToStore = { Logging.Default with Info = writeToStore }

  let internal GetStringValue s =
    writeToStore String.Empty
    LogToStore.Apply()
    [| s |]
    |> Main.EffectiveMain
    |> ignore
    store

  let Ipmo() = GetStringValue "ipmo"
  let Version() = GetStringValue "version"

type Prepare() =
  inherit Task(null)
  member val InputDirectory = String.Empty with get, set
  member val OutputDirectory = String.Empty with get, set
  member val SymbolDirectories : string array = [||] with get, set
#if NETCOREAPP2_0
  member val Dependencies : string array = [||] with get, set
#else
  member val Keys : string array = [| |] with get, set
  member val StrongNameKey = String.Empty with get, set
#endif
  member val XmlReport = String.Empty with get, set
  member val FileFilter : string array = [||] with get, set
  member val AssemblyFilter : string array = [||] with get, set
  member val AssemblyExcludeFilter : string array = [||] with get, set
  member val TypeFilter : string array = [||] with get, set
  member val MethodFilter : string array = [||] with get, set
  member val AttributeFilter : string array = [||] with get, set
  member val PathFilter : string array = [||] with get, set
  member val CallContext : string array = [||] with get, set
  member val OpenCover = true with get, set
  member val InPlace = true with get, set
  member val Save = true with get, set
  member val Single = true |> not with get, set // work around Gendarme insistence on non-default values only
  member val LineCover = true |> not with get, set
  member val BranchCover = true |> not with get, set
  [<Obsolete("Please use AltCover.Prepare.Command instead.")>]
  member val CommandLine = String.Empty with get, set
  member val Command : string array = [||] with get, set
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      { Logging.Default with Error = base.Log.LogError
                             Warn = base.Log.LogWarning
                             Info = self.Message }

    let task =
      { PrepareParameters.Create() with InputDirectory = DirectoryPath self.InputDirectory
                                        OutputDirectory = DirectoryPath self.OutputDirectory
                                        SymbolDirectories = self.SymbolDirectories
                                                            |>Seq.map DirectoryPath
                                                            |> DirectoryPaths
#if NETCOREAPP2_0
                                        Dependencies = self.Dependencies
                                                       |> Seq.map FilePath
                                                       |> FilePaths
#else
                                        Keys = self.Keys
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
                                        OpenCover = Flag self.OpenCover
                                        InPlace = Flag self.InPlace
                                        Save = Flag self.Save
                                        Single = Flag self.Single
                                        LineCover = Flag self.LineCover
                                        BranchCover = Flag self.BranchCover
                                        CommandLine = if self.Command |> Seq.isEmpty |> not then
                                                         self.Command
                                                         |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                                                         |> Seq.map CommandArgument |> Command
                                                      else CommandArgs.extract self.CommandLine }

    Api.DoPrepare task log = 0

type Collect() =
  inherit Task(null)

  [<Required>]
  member val RecorderDirectory = String.Empty with get, set

  member val WorkingDirectory = String.Empty with get, set
  member val Executable = String.Empty with get, set
  member val LcovReport = String.Empty with get, set
  member val Threshold = String.Empty with get, set
  member val Cobertura = String.Empty with get, set
  member val OutputFile = String.Empty with get, set
  [<Obsolete("Please use AltCover.Collect.Command instead.")>]
  member val CommandLine = String.Empty with get, set
  member val Command : string array = [||] with get, set
  member self.Message x = base.Log.LogMessage(MessageImportance.High, x)
  override self.Execute() =
    let log =
      { Logging.Default with Error = base.Log.LogError
                             Warn = base.Log.LogWarning
                             Info = self.Message }

    let task =
      { CollectParameters.Create() with RecorderDirectory = DirectoryPath self.RecorderDirectory
                                        WorkingDirectory = DirectoryPath self.WorkingDirectory
                                        Executable = FilePath self.Executable
                                        LcovReport = FilePath self.LcovReport
                                        Threshold = match Byte.TryParse self.Threshold with
                                                    | (true, n) -> Threshold n
                                                    | _ -> NoThreshold
                                        Cobertura = FilePath self.Cobertura
                                        OutputFile = FilePath self.OutputFile
                                        CommandLine = if self.Command |> Seq.isEmpty |> not then
                                                         self.Command
                                                         |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                                                         |> Seq.map CommandArgument |> Command
                                                      else CommandArgs.extract self.CommandLine }
    Api.DoCollect task log = 0

type PowerShell() =
  inherit Task(null)

  member val internal IO = { Logging.Default with Error = base.Log.LogError
                                                  Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Ipmo()
    self.IO.Apply()
    r |> Output.Warn
    true

type GetVersion() =
  inherit Task(null)

  member val internal IO = { Logging.Default with Error = base.Log.LogError
                                                  Warn = base.Log.LogWarning } with get, set

  override self.Execute() =
    let r = Api.Version()
    self.IO.Apply()
    r |> Output.Warn
    true