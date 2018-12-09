#if RUNNER
namespace AltCover
#else
[<RequireQualifiedAccess>]
module AltCover_Fake.DotNet.Testing.AltCover
#endif

open System
open System.Diagnostics.CodeAnalysis
open System.Linq
open BlackFox.CommandLine
#if RUNNER
open AltCover.Augment
#else
open System.Reflection
open System.IO
open Fake.Core
open System.Globalization

#endif

// No more primitive obsession!
[<ExcludeFromCodeCoverage; NoComparison>]
type FilePath = FilePath of String
                | Unset
                member self.AsString () =
                  match self with
                  | Unset -> String.Empty
                  | FilePath s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type CommandArgument = | CommandArgument of String
                         member self.AsString () =
                            match self with
                            | CommandArgument s -> s

[<ExcludeFromCodeCoverage; NoComparison>]
type Command = Command of CommandArgument seq
               | NoCommand
               member self.AsStrings () =
                  match self with
                  | NoCommand -> Seq.empty<String>
                  | Command c -> c |> Seq.map (fun a -> a.AsString())

[<ExcludeFromCodeCoverage; NoComparison>]
type Threshold = Threshold of uint8
                 | NoThreshold
                  member self.AsString () =
                    match self with
                    | NoThreshold -> String.Empty
                    | Threshold t -> t.ToString(CultureInfo.InvariantCulture)

[<ExcludeFromCodeCoverage; NoComparison>]
type Flag = Flag of bool | Set | Clear
                  member self.AsBool () =
                    match self with
                    | Set -> true
                    | Clear -> false
                    | Flag b -> b

[<ExcludeFromCodeCoverage; NoComparison>]
type FilePaths = FilePaths of FilePath seq
                | NoPaths
[<ExcludeFromCodeCoverage; NoComparison>]
type FilterItem = FilterItem of System.Text.RegularExpressions.Regex
[<ExcludeFromCodeCoverage; NoComparison>]
type Filters = Filters of FilterItem seq
               | Unfiltered
[<ExcludeFromCodeCoverage; NoComparison>]
type ContextItem = CallItem of String
                   | TimeItem of uint8
[<ExcludeFromCodeCoverage; NoComparison>]
type Context = Context of ContextItem seq
               | NoContext

module internal CommandArgs =
  let internal parse s =
    let blackfox = typeof<CmdLine>.Assembly
    let t = blackfox.GetType(if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
              then "BlackFox.CommandLine.MsvcrCommandLine"
              else "BlackFox.CommandLine.MonoUnixCommandLine")
    let m = t.GetMethod "parse"
    m.Invoke (m, [| s |]) :?> String seq |> Seq.toList

  let internal extract command commandLine =
    let commands = command
                   |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                   |> Seq.toList
    let args = if List.isEmpty commands then
                  parse commandLine
               else commands
    if List.isEmpty args then
        NoCommand
    else args |> List.map CommandArgument |> List.toSeq |> Command

[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParameters =
  { RecorderDirectory : FilePath
    WorkingDirectory : FilePath
    Executable : FilePath
    LcovReport : FilePath
    Threshold : Threshold
    Cobertura : FilePath
    OutputFile : FilePath
    CommandLine : Command }
  static member Create() =
    { RecorderDirectory = Unset
      WorkingDirectory = Unset
      Executable = Unset
      LcovReport = Unset
      Threshold = NoThreshold
      Cobertura = Unset
      OutputFile = Unset
      CommandLine = NoCommand }

#if RUNNER
  member self.Validate afterPreparation =
    let saved = CommandLine.error

    let validate f x =
      if x
         |> String.IsNullOrWhiteSpace
         |> not
      then f x |> ignore

    let validateOptional f key x = validate (f key) x

    let toOption s =
      if s |> String.IsNullOrWhiteSpace then None
      else Some s
    try
      [ ("--recorderDirectory", self.RecorderDirectory)
        ("--workingDirectory", self.WorkingDirectory) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidateDirectory n x)
      [ ("--executable", self.Executable)
        ("--lcovReport", self.LcovReport)
        ("--cobertura", self.Cobertura)
        ("--outputFile", self.OutputFile) ]
      |> List.iter (fun (n, x) -> validateOptional CommandLine.ValidatePath n x)
      validate Runner.ValidateThreshold self.Threshold
      if afterPreparation then
        Runner.RequireRecorderTest (self.RecorderDirectory |> toOption) () ()
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved
#else
  member self.withCommandLine args =
    { self with CommandLine = args |> Seq.map CommandArgument |> Command}
#endif

[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParameters =
  { InputDirectory : FilePath
    OutputDirectory : FilePath
    SymbolDirectories : FilePaths
    Dependencies : FilePaths
    Keys : FilePaths
    StrongNameKey : FilePath
    XmlReport : FilePath
    FileFilter : Filters
    AssemblyFilter : Filters
    AssemblyExcludeFilter : Filters
    TypeFilter : Filters
    MethodFilter : Filters
    AttributeFilter : Filters
    PathFilter : Filters
    CallContext : Context
    OpenCover : Flag
    InPlace : Flag
    Save : Flag
    Single : Flag
    LineCover : Flag
    BranchCover : Flag
    CommandLine : Command }
  static member Create() =
    { InputDirectory = Unset
      OutputDirectory = Unset
      SymbolDirectories = NoPaths
      Dependencies = NoPaths
      Keys = NoPaths
      StrongNameKey = Unset
      XmlReport = Unset
      FileFilter = Unfiltered
      AssemblyFilter = Unfiltered
      AssemblyExcludeFilter = Unfiltered
      TypeFilter = Unfiltered
      MethodFilter = Unfiltered
      AttributeFilter = Unfiltered
      PathFilter = Unfiltered
      CallContext = NoContext
      OpenCover = Set
      InPlace = Set
      Save = Set
      Single = Clear
      LineCover = Clear
      BranchCover = Clear
      CommandLine = NoCommand }

#if RUNNER
  static member private validateArray a f key =
    PrepareParams.validateArraySimple a (f key)

  static member private nonNull a =
    a
    |> isNull
    |> not

  [<SuppressMessage("Microsoft.Usage", "CA2208",
                    Justification = "Some in-lined code must be creating an ArgumentNullException")>]
  static member private validateArraySimple a f =
    if a |> PrepareParams.nonNull then a |> Seq.iter (fun s -> f s |> ignore)

  static member private validateOptional f key x =
    if x
       |> String.IsNullOrWhiteSpace
       |> not
    then f key x |> ignore

  member private self.consistent() =
    if self.Single && self.CallContext |> PrepareParams.nonNull && self.CallContext.Any() then
      CommandLine.error <- String.Format
                             (System.Globalization.CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString "Incompatible", "--single",
                              "--callContext") :: CommandLine.error

  member private self.consistent'() =
    if self.LineCover && self.BranchCover then
      CommandLine.error <- String.Format
                             (System.Globalization.CultureInfo.CurrentCulture,
                              CommandLine.resources.GetString "Incompatible",
                              "--branchcover", "--linecover") :: CommandLine.error

  member self.Validate() =
    let saved = CommandLine.error

    let validateContext context =
      if context
         |> isNull
         |> not
      then
        let select state x =
          let (_, n) = Main.ValidateCallContext state x
          match (state, n) with
          | (true, _) | (_, Left(Some _)) -> true
          | _ -> false
        context
        |> Seq.fold select false
        |> ignore
    try
      CommandLine.error <- []
      PrepareParams.validateOptional CommandLine.ValidateDirectory "--inputDirectory"
        self.InputDirectory
      PrepareParams.validateOptional CommandLine.ValidatePath "--outputDirectory"
        self.OutputDirectory
      PrepareParams.validateOptional CommandLine.ValidateStrongNameKey "--strongNameKey"
        self.StrongNameKey
      PrepareParams.validateOptional CommandLine.ValidatePath "--xmlReport" self.XmlReport
      PrepareParams.validateArray self.SymbolDirectories CommandLine.ValidateDirectory
        "--symbolDirectory"
      PrepareParams.validateArray self.Dependencies CommandLine.ValidateAssembly
        "--dependency"
      PrepareParams.validateArray self.Keys CommandLine.ValidateStrongNameKey "--key"
      [ self.FileFilter; self.AssemblyFilter; self.AssemblyExcludeFilter; self.TypeFilter;
        self.MethodFilter; self.AttributeFilter; self.PathFilter ]
      |> Seq.iter
           (fun a -> PrepareParams.validateArraySimple a CommandLine.ValidateRegexes)
      self.consistent()
      self.consistent'()
      validateContext self.CallContext
      CommandLine.error |> List.toArray
    finally
      CommandLine.error <- saved
#else
  member self.withCommandLine args =
    { self with CommandLine = args |> Seq.map CommandArgument |> Command}
#endif

[<Obsolete("Please use AltCover.CollectParameters instead.")>]
[<ExcludeFromCodeCoverage; NoComparison>]
type CollectParams =
  { RecorderDirectory : String
    WorkingDirectory : String
    Executable : String
    LcovReport : String
    Threshold : String
    Cobertura : String
    OutputFile : String
    CommandLine : String
    Command : String seq }

#if RUNNER
  [<Obsolete("Please use AltCover.CollectParams.Create() instead.")>]
  static member Default : CollectParams =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty
      Command = [] }
#endif

  static member Create() =
    { RecorderDirectory = String.Empty
      WorkingDirectory = String.Empty
      Executable = String.Empty
      LcovReport = String.Empty
      Threshold = String.Empty
      Cobertura = String.Empty
      OutputFile = String.Empty
      CommandLine = String.Empty
      Command = [] }

  member self.ToParameters () =
    { CollectParameters.Create() with RecorderDirectory = FilePath self.RecorderDirectory
                                      WorkingDirectory = FilePath self.WorkingDirectory
                                      Executable = FilePath self.Executable
                                      LcovReport = FilePath self.LcovReport
                                      Threshold = match uint8.TryParse self.Threshold with
                                                  | (true, n) -> Threshold n
                                                  | _ -> NoThreshold
                                      Cobertura = FilePath self.Cobertura
                                      OutputFile = FilePath self.OutputFile
                                      CommandLine = CommandArgs.extract self.Command self.CommandLine }
#if RUNNER
  member self.Validate afterPreparation =
    self.ToParamaters().Validate afterPreparation
#else
  member self.withCommandLine args =
    { self with Command = args
                CommandLine = String.Empty}
#endif

[<Obsolete("Please use AltCover.PerpareParameters instead.")>]
[<ExcludeFromCodeCoverage; NoComparison>]
type PrepareParams =
  { InputDirectory : String
    OutputDirectory : String
    SymbolDirectories : string seq
    Dependencies : string seq
    Keys : string seq
    StrongNameKey : String
    XmlReport : String
    FileFilter : string seq
    AssemblyFilter : string seq
    AssemblyExcludeFilter : string seq
    TypeFilter : string seq
    MethodFilter : string seq
    AttributeFilter : string seq
    PathFilter : string seq
    CallContext : string seq
    OpenCover : bool
    InPlace : bool
    Save : bool
    Single : bool
    LineCover : bool
    BranchCover : bool
    // [<Obs // olete("Please use AltCover.PrepareParams.Command instead.")>]
    CommandLine : String
    Command : String seq }

  member self.ToParameters () = // TODO
    { PrepareParameters.Create() with InputDirectory = FilePath self.InputDirectory }

#if RUNNER
  [<Obsolete("Please use AltCover.CollectParams.Create() instead.")>]
  static member Default : PrepareParams =
    { InputDirectory = String.Empty
      OutputDirectory = String.Empty
      SymbolDirectories = [||]
      Dependencies = [||]
      Keys = [||]
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = [||]
      AssemblyFilter = [||]
      AssemblyExcludeFilter = [||]
      TypeFilter = [||]
      MethodFilter = [||]
      AttributeFilter = [||]
      PathFilter = [||]
      CallContext = [||]
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = String.Empty
      Command = [] }
#endif

  static member Create() =
    { InputDirectory = String.Empty
      OutputDirectory = String.Empty
      SymbolDirectories = Seq.empty
      Dependencies = Seq.empty
      Keys = Seq.empty
      StrongNameKey = String.Empty
      XmlReport = String.Empty
      FileFilter = Seq.empty
      AssemblyFilter = Seq.empty
      AssemblyExcludeFilter = Seq.empty
      TypeFilter = Seq.empty
      MethodFilter = Seq.empty
      AttributeFilter = Seq.empty
      PathFilter = Seq.empty
      CallContext = Seq.empty
      OpenCover = true
      InPlace = true
      Save = true
      Single = false
      LineCover = false
      BranchCover = false
      CommandLine = String.Empty
      Command = [] }

#if RUNNER
  member self.Validate() =
    self.ToParamaters().Validate ()
#else
  member self.withCommandLine args =
    { self with Command = args
                CommandLine = String.Empty}
#endif

module internal Args =
  let private Item a x =
    if x |> String.IsNullOrWhiteSpace then []
    else [ a; x ]

  let internal ItemList a x =
    if x |> isNull then []
    else
      x
      |> Seq.collect (fun i -> [ a; i ])
      |> Seq.toList

  let private Flag a x =
    if x then [ a ]
    else []

  let Prepare(args : PrepareParameters) =
    let argsList = args.CommandLine.AsStrings() |> Seq.toList
    let trailing = if List.isEmpty argsList then []
                   else "--" :: argsList

    [ Item "-i" <| args.InputDirectory.AsString()
      Item "-o" <| args.OutputDirectory.AsString()
      ItemList "-y" args.SymbolDirectories
      ItemList "-d" args.Dependencies
      ItemList "-k" args.Keys
      Item "--sn" <| args.StrongNameKey.AsString()
      Item "-x" <| args.XmlReport.AsString()
      ItemList "-f" args.FileFilter
      ItemList "-s" args.AssemblyFilter
      ItemList "-e" args.AssemblyExcludeFilter
      ItemList "-t" args.TypeFilter
      ItemList "-m" args.MethodFilter
      ItemList "-a" args.AttributeFilter
      ItemList "-p" args.PathFilter
      ItemList "-c" args.CallContext
      Flag "--opencover" <| args.OpenCover.AsBool()
      Flag "--inplace" <| args.InPlace.AsBool()
      Flag "--save" <| args.Save.AsBool()
      Flag "--single" <| args.Single.AsBool()
      Flag "--linecover" <| args.LineCover.AsBool()
      Flag "--branchcover" <| args.BranchCover.AsBool()
      trailing ]
    |> List.concat

  let Collect(args : CollectParameters) =
    let argsList = args.CommandLine.AsStrings() |> Seq.toList
    let trailing = if List.isEmpty argsList then []
                   else "--" :: argsList

    [ [ "Runner" ]
      Item "-r" <| args.RecorderDirectory.AsString()
      Item "-w" <| args.WorkingDirectory.AsString()
      Item "-x" <| args.Executable.AsString()
      Item "-l" <| args.LcovReport.AsString()
      Item "-t" <| args.Threshold.AsString()
      Item "-c" <| args.Cobertura.AsString()
      Item "-o" <| args.OutputFile.AsString()
      Flag "--collect" (args.Executable.AsString() |> String.IsNullOrWhiteSpace)
      trailing ]
    |> List.concat

#if RUNNER
#else
[<NoComparison>]
type ArgType =
  | [<Obsolete("Please use Collecting of AltCover.CollectParameters instead.")>]
    Collect of CollectParams
  | [<Obsolete("Please use Preparing of AltCover.PrepareParameters instead.")>]
    Prepare of PrepareParams
  | Collecting of CollectParameters
  | Preparing of PrepareParameters
  | ImportModule
  | GetVersion

[<NoComparison>]
type ToolType =
  | DotNet of string option
  | Mono of string option
  | Global
  | Framework

[<NoComparison>]
type Params =
  { /// Path to the Altcover executable.
    ToolPath : string
    /// Which version of the tool
    ToolType : ToolType
    /// Working directory for relative file paths.  Default is the current working directory
    WorkingDirectory : string
    /// Command arguments
    Args : ArgType }

  static member Create (a:ArgType) =
    {
        ToolPath = "altcover"
        ToolType = Global
        WorkingDirectory = String.Empty
        Args = a
    }
let internal createArgs parameters =
  match parameters.Args with
  | Collect c -> Args.Collect <| c.ToParameters()
  | Collecting c -> Args.Collect c
  | Prepare p ->
    (match parameters.ToolType with
     | Framework
     | Mono _ -> { p with Dependencies = Seq.empty }
     | _ -> { p with Keys = Seq.empty
                     StrongNameKey = String.Empty}).ToParameters()
     |> Args.Prepare
  | Preparing p ->
    (match parameters.ToolType with
     | Framework
     | Mono _ -> { p with Dependencies = NoPaths }
     | _ -> { p with Keys = NoPaths
                     StrongNameKey = Unset})
     |> Args.Prepare
  | ImportModule -> [ "ipmo" ]
  | GetVersion -> [ "version" ]

let internal createProcess parameters args =
  let baseline () = CreateProcess.fromRawCommand parameters.ToolPath args
  match parameters.ToolType with
  | Framework -> baseline () |> CreateProcess.withFramework
  | Global -> baseline ()
  | DotNet dotnetPath ->
       let path =
         match dotnetPath with
         | None -> "dotnet"
         | Some p -> p
       CreateProcess.fromRawCommand path (parameters.ToolPath::args)
  | Mono monoPath ->
       let path =
         match monoPath with
         | None -> "mono"
         | Some p -> p
       CreateProcess.fromRawCommand path ("--debug"::parameters.ToolPath::args)
  |> if String.IsNullOrWhiteSpace parameters.WorkingDirectory then id
     else CreateProcess.withWorkingDirectory parameters.WorkingDirectory

let composeCommandLine parameters =
  let args = createArgs parameters
  createProcess parameters args

let run parameters =
  use __ = Trace.traceTask "AltCover" String.Empty
  let command = composeCommandLine parameters
  let run = command |> Proc.run
  if 0 <> run.ExitCode then failwithf "AltCover '%s' failed." command.CommandLine
  __.MarkSuccess()
#endif