namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Threading

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
           { this with Stream = s
                       Formatter = new BinaryWriter(s)
                       Runner = true })
      |> Seq.head
    else this

  member this.Close() =
    try
      this.Stream.Flush()
      this.Formatter.Close()
    with :? ObjectDisposedException -> ()

  member private this.PushContext context =
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
    | Table t ->
      this.Formatter.Write(Tag.Table |> byte)
      t.Keys
      |> Seq.iter (fun m -> this.Formatter.Write m
                            t.[m].Keys
                            |> Seq.iter (fun p -> this.Formatter.Write p
                                                  let v = t.[m].[p]
                                                  this.Formatter.Write v.Count
                                                  v.Tracks
                                                  |> Seq.iter this.PushContext
                                                  this.PushContext Null)
                            this.Formatter.Write 0)
      this.Formatter.Write String.Empty

  member internal this.Push (moduleId : string) (hitPointId : int) context =
    this.Formatter.Write moduleId
    this.Formatter.Write hitPointId
    this.PushContext context

  member internal this.CatchUp(visits : Dictionary<string, Dictionary<int, PointVisit>> array)
                              (vlock : ReaderWriterLock) =
    if visits.[0].Count > 0 then
      let empty = Null
      let dict = Dictionary<string, Dictionary<int, PointVisit>> ()
      let counts = try
                      vlock.AcquireWriterLock(-1)
                      System.Threading.Interlocked.Exchange(&visits.[0], dict)
                   finally
                      vlock.ReleaseWriterLock()
      counts |> Table |> this.Push String.Empty 0

  member this.OnStart() =
    let running =
      if this.Tracer <> "Coverage.Default.xml.acv" then this.Connect()
      else this
    { running with Definitive = true }

  member this.OnConnected f g =
    if this.IsConnected() then f()
    else g()

  member internal this.OnFinish finish visits vlock =
    this.CatchUp visits vlock
    if finish <> Pause
    then this.Close()

  member internal this.OnVisit visits vlock moduleId hitPointId context =
    this.CatchUp visits vlock
    this.Push moduleId hitPointId context
    this.Formatter.Flush()
    this.Stream.Flush()