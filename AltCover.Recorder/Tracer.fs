namespace AltCover.Recorder

open System.Collections.Generic
open System.IO
open System.IO.Compression
open System

#if NETSTANDARD2_0
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#else
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume

module Extra =
  let streams = System.Collections.ObjectModel.Collection<Stream * BinaryWriter>()

[<NoComparison>]
type Tracer =
  { Tracer : string
    Runner : bool
    Definitive : bool
    Stream : System.IO.Stream
    Formatter : System.IO.BinaryWriter }
#if NETSTANDARD2_0
  static member Core() =
    typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
#endif

  static member Create(name : string) =
    { Tracer = name
      Runner = false
      Definitive = false
      Stream = null
      Formatter = null }

  member this.IsDefiniteRunner() =
    this.Runner && this.Definitive

  member this.IsConnected() =
    match this.Stream with
    | null -> false
    | _ -> this.Runner

  member this.Connect() =
    if File.Exists this.Tracer then
      Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer, sprintf ".%d.acv" i))
      |> Seq.filter (File.Exists >> not)
      |> Seq.map (fun f ->
           let fs = File.OpenWrite f
           let s = new DeflateStream(fs, CompressionMode.Compress)
           let fo = new BinaryWriter(s)
           Extra.streams.Add(s :> Stream, fo)
           { this with Stream = s
                       Formatter = fo
                       Runner = true })
      |> Seq.head
    else this
    (* 105, 106 exist
The active test run was aborted. Reason: Unhandled Exception: System.IO.IOException: The process cannot access the file 'C:\Users\steve\Documents\GitHub\sawdust-master\sd_tests\coverage.xml.105.acv' because it is being used by another process.
   at System.IO.FileStream.ValidateFileHandle(SafeFileHandle fileHandle)
   at System.IO.FileStream.CreateFileOpenHandle(FileMode mode, FileShare share, FileOptions options)
   at System.IO.FileStream..ctor(String path, FileMode mode, FileAccess access, FileShare share, Int32 bufferSize, FileOptions options)
   at <StartupCode$AltCover-Recorder>.$Tracer.Connect@53.Invoke(String f)
   at Microsoft.FSharp.Collections.Internal.IEnumerator.map@74.DoMoveNext(b& curr)
   at Microsoft.FSharp.Collections.Internal.IEnumerator.MapEnumerator`1.System-Collections-IEnumerator-MoveNext()
   at Microsoft.FSharp.Collections.SeqModule.Head[T](IEnumerable`1 source)
   at AltCover.Recorder.Tracer.OnStart()
   at AltCover.Recorder.Instance.WithMutex[a](FSharpFunc`2 f)
   at AltCover.Recorder.Instance.get_Instance@135.Invoke(Boolean _arg2)
   at AltCover.Recorder.Instance.WithMutex[a](FSharpFunc`2 f)
   at AltCover.Recorder.Instance.TraceOut.get_Instance()
   at AltCover.Recorder.Instance.VisitImpl(String moduleId, Int32 hitPointId, Track context)
   at EricSinkMultiCoreLib.multicore.<>c__DisplayClass7_2`2.<Map_ResultsIndexed>b__0(Object obj) in C:\Users\steve\Documents\GitHub\sawdust-master\sd\multicore.cs:line 196
   at System.Threading.QueueUserWorkItemCallback.Execute()
   at System.Threading.ThreadPoolWorkQueue.Dispatch()
    *)

  static member Close() =
    Extra.streams |> Seq.iter (fun (s,f) ->
        try
          s.Flush()
          f.Close()
        with :? ObjectDisposedException -> ())

  member internal this.Push (moduleId : string) (hitPointId : int) context =
    this.Formatter.Write moduleId
    this.Formatter.Write hitPointId
    match context with
    | Null -> this.Formatter.Write(Tag.Null |> byte)
    | Time t ->
      this.Formatter.Write(Tag.Time |> byte)
      this.Formatter.Write(t)
    | Call t ->
      this.Formatter.Write(Tag.Call |> byte)
      this.Formatter.Write(t)
    | Both(t', t) ->
      this.Formatter.Write(Tag.Both |> byte)
      this.Formatter.Write(t')
      this.Formatter.Write(t)

  member internal this.CatchUp(visits : Dictionary<string, Dictionary<int, int * Track list>>) =
    let empty = Null
    visits.Keys
    |> Seq.iter (fun moduleId ->
         let m = visits.[moduleId]
         m.Keys
         |> Seq.iter (fun hitPointId ->
              let n, l = m.[hitPointId]
              let push = this.Push moduleId hitPointId
              [ seq { 1..n } |> Seq.map (fun _ -> empty)
                l |> List.toSeq ]
              |> Seq.concat
              |> Seq.iter push))
    visits.Clear()

  member this.OnStart() =
    let running =
      if this.Tracer <> "Coverage.Default.xml.acv" then this.Connect()
      else this
    { running with Definitive = true }

  member this.OnConnected f g =
    if this.IsConnected() then f()
    else g()

  member internal this.OnFinish finish visits =
    this.CatchUp visits
    if finish <> Pause
    then Tracer.Close()

  member internal this.OnVisit visits moduleId hitPointId context =
    this.CatchUp visits
    this.Push moduleId hitPointId context
    this.Formatter.Flush()
    this.Stream.Flush()