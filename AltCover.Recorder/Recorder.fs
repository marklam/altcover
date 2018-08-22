// Based upon C# code by Sergiy Sakharov (sakharov@gmail.com)
// http://code.google.com/p/dot-net-coverage/source/browse/trunk/Coverage.Counter/Coverage.Counter.csproj

namespace AltCover.Recorder

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Resources
open System.Runtime.CompilerServices

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

#if NETSTANDARD2_0
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#else
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#endif
type internal Carrier =
    | SequencePoint of String*int*Track

#if NETSTANDARD2_0
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#else
[<System.Runtime.InteropServices.ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#endif
type internal Message =
    | AsyncItem of Carrier seq
    | Item of Carrier seq*AsyncReplyChannel<unit>
    | Finish of Close * AsyncReplyChannel<unit>

module Instance =

  let internal resources = ResourceManager("AltCover.Recorder.Strings" , Assembly.GetExecutingAssembly())

  let GetResource s =
    [
      System.Globalization.CultureInfo.CurrentUICulture.Name
      System.Globalization.CultureInfo.CurrentUICulture.Parent.Name
      "en"
    ]
    |> Seq.map (fun l -> resources.GetString(s + "." + l))
    |> Seq.tryFind (String.IsNullOrEmpty >> not)

  /// <summary>
  /// Gets the location of coverage xml file
  /// This property's IL code is modified to store actual file location
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let ReportFile = "Coverage.Default.xml"

  /// <summary>
  /// Accumulation of visit records
  /// </summary>
  let internal Visits = new Dictionary<string, Dictionary<int, int * Track list>>()
  let internal Samples = new Dictionary<string, Dictionary<int, bool>>()
  let internal buffer = List<Carrier> ()

  /// <summary>
  /// Gets the unique token for this instance
  /// This property's IL code is modified to store a GUID-based token
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Token = "AltCover"

  /// <summary>
  /// Gets the style of the associated report
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let internal CoverageFormat = ReportFormat.NCover

  /// <summary>
  /// Gets the frequency of time sampling
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let Timer = 0L

  /// <summary>
  /// Gets the sampling strategy
  /// This property's IL code is modified to store the user chosen override if applicable
  /// </summary>
  [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
  let internal Sample = Sampling.All

  /// <summary>
  /// Gets or sets the current test method
  /// </summary>
  type private CallStack =
    [<ThreadStatic;DefaultValue>]
    static val mutable private instance:Option<CallStack>

    val mutable private caller:int list
    private new (x:int) = {caller = [x]}

    static member Instance =
        match CallStack.instance with
        | None -> CallStack.instance <- Some (CallStack(0))
        | _ -> ()

        CallStack.instance.Value

    member self.Push x =  self.caller <- x :: self.caller

    member self.Pop () = self.caller <- match self.caller with
                                         | []
                                         | [0] -> [0]
                                         | _::xs -> xs

    member self.CallerId () = Seq.head self.caller

  let Push x = CallStack.Instance.Push x
  let Pop () = CallStack.Instance.Pop ()
  let CallerId () = CallStack.Instance.CallerId ()

  /// <summary>
  /// Serialize access to the report file across AppDomains for the classic mode
  /// </summary>
  let internal mutex = new System.Threading.Mutex(false, Token + ".mutex");

  let SignalFile () = ReportFile + ".acv"

  /// <summary>
  /// Reporting back to the mother-ship
  /// </summary>
  let mutable internal trace = Tracer.Create (SignalFile ())

  let internal WithMutex (f : bool -> 'a) =
    let own = mutex.WaitOne(1000)
    try
      f(own)
    finally
      if own then mutex.ReleaseMutex()

  let InitialiseTrace () =
    WithMutex (fun _ -> let t = Tracer.Create (SignalFile ())
                        trace <- t.OnStart ())

  let internal Watcher = new FileSystemWatcher()
  let mutable internal Recording = true

  /// <summary>
  /// This method flushes hit count buffers.
  /// </summary>
  let internal FlushAll () =
    trace.OnConnected (fun () -> trace.OnFinish Visits)
      (fun () ->
      match Visits.Count with
      | 0 -> ()
      | _ -> let counts = Dictionary<string, Dictionary<int, int * Track list>> Visits
             Visits.Clear()
             WithMutex (fun own ->
                let delta = Counter.DoFlush ignore (fun _ _ -> ()) own counts CoverageFormat ReportFile None
                GetResource "Coverage statistics flushing took {0:N} seconds"
                |> Option.iter (fun s -> Console.Out.WriteLine(s, delta.TotalSeconds))
             ))

  let FlushPause () =
    ("PauseHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    FlushAll ()
    InitialiseTrace()

  let FlushResume () =
    ("ResumeHandler")
    |> GetResource
    |> Option.iter Console.Out.WriteLine
    Visits.Clear()
    InitialiseTrace ()

  let internal TraceVisit moduleId hitPointId context =
     trace.OnVisit Visits moduleId hitPointId context

  let internal TakeSample strategy moduleId hitPointId =
    match strategy with
    | Sampling.All -> true
    | _ -> let hasKey = Samples.ContainsKey(moduleId)
           if hasKey |> not then Samples.Add(moduleId, Dictionary<int, bool>())
           let unwanted = hasKey && Samples.[moduleId].ContainsKey(hitPointId)
           let wanted = unwanted |> not
           if wanted then Samples.[moduleId].Add(hitPointId, true)
           wanted

  /// <summary>
  /// This method is executed from instrumented assemblies.
  /// </summary>
  /// <param name="moduleId">Assembly being visited</param>
  /// <param name="hitPointId">Sequence Point identifier</param>
  let internal VisitImpl moduleId hitPointId context =
    if not <| String.IsNullOrEmpty(moduleId) &&
       TakeSample Sample moduleId hitPointId then
      trace.OnConnected (fun () -> TraceVisit moduleId hitPointId context)
                        (fun () -> Counter.AddVisit Visits moduleId hitPointId context)

  let Fault _ = async { InvalidOperationException() |> raise }
  let internal MakeDefaultMailbox() =
    new MailboxProcessor<Message>(Fault)

  let mutable internal mailbox = MakeDefaultMailbox()
  let mutable internal mailboxOK = false

  let internal Post (x : Carrier) =
    match x with
    | SequencePoint (moduleId, hitPointId, context) ->
      VisitImpl moduleId hitPointId context

  let mutable internal closedown = false

  let CoverageFlushed = "Coverage flushed" |> GetResource |> Option.get

  let latch = new System.Threading.ManualResetEvent(false)

  let internal DispatchVisit msg =
    System.Diagnostics.Debug.WriteLine ("Dispatching " + msg.ToString())
    match msg with
    | AsyncItem s ->
        s |>
        Seq.iter Post
        trace.OnConnected (fun () -> trace.Formatter.Flush()
                                     if File.Exists trace.Tracer then
                                       System.Diagnostics.Debug.WriteLine ("trace = " + FileInfo(trace.Tracer).Length.ToString())
                                                 )
                          ignore
        true
    | Item (s, channel) ->
        s |>
        Seq.iter Post
        trace.OnConnected (fun () -> trace.Formatter.Flush()
                                     if File.Exists trace.Tracer then
                                       System.Diagnostics.Debug.WriteLine ("trace = " + FileInfo(trace.Tracer).Length.ToString())
                                                 )
                          ignore
        channel.Reply ()
        true
    | Finish (Pause, channel) ->
        FlushPause()
        channel.Reply ()
        true
    | Finish (Resume, channel) ->
        FlushResume ()
        channel.Reply ()
        true
    | Finish (_, channel) ->
        FlushAll ()
        //printfn "%s" CoverageFlushed
        CoverageFlushed |> System.Diagnostics.Debug.WriteLine
        channel.Reply ()
        mailboxOK <- false
        Assist.SafeDispose mailbox
        latch.Set() |> ignore
        false

  let rec private loop (main:bool) (inbox:MailboxProcessor<Message>) =
    async {
      System.Diagnostics.Debug.WriteLine "loop"
      if Object.ReferenceEquals (inbox, mailbox) &&
         (main && closedown) |> not &&
         mailboxOK then
        // release the wait every half second
        let! opt = inbox.TryReceive(500)
        match opt with
        | None -> return! loop main inbox
        | Some msg ->
          if DispatchVisit msg then
           return! loop main inbox
        }

  let internal MakeMailbox () =
    new MailboxProcessor<Message>(loop true)

  let internal Backlog () =
    mailbox.CurrentQueueLength

  let internal DefaultErrorAction = ignore
  let mutable internal ErrorAction = DefaultErrorAction

  let MailboxError x =
    System.Diagnostics.Debug.WriteLine ("MailboxError " + x.ToString())
    mailboxOK <- false
    ErrorAction x

  let DisplayError x =
    eprintfn "%s - %A" ("Recorder error" |> GetResource |> Option.get) x

  let private IsOpenCoverRunner() =
     (CoverageFormat = ReportFormat.OpenCoverWithTracking) &&
       ((trace.Definitive && trace.Runner) ||
        (ReportFile <> "Coverage.Default.xml" && System.IO.File.Exists (ReportFile + ".acv")))

  let internal Granularity() = Timer

  let internal Clock () = DateTime.UtcNow.Ticks

  let internal PayloadSelection clock frequency wantPayload =
    if wantPayload () then
       match (frequency(), CallerId()) with
       | (0L, 0) -> Null
       | (t, 0) -> Time (t*(clock()/t))
       | (0L, n) -> Call n
       | (t, n) -> Both (t*(clock()/t), n)
    else Null

  let internal PayloadControl = PayloadSelection Clock

  let internal PayloadSelector enable =
    PayloadControl Granularity enable

  let mutable internal Capacity = 1

  let UnbufferedVisit (f: unit -> bool)  =
    if f() then
     System.Diagnostics.Debug.WriteLine "unbuffered +"
     mailbox.PostAndReply (fun c -> Item (buffer |> Seq.toArray, c))
    else System.Diagnostics.Debug.WriteLine "unbuffered -"
         buffer |> Seq.toArray |> Array.toSeq |> AsyncItem |> mailbox.Post

  let internal VisitSelection (f: unit -> bool) track moduleId hitPointId =
    // When writing to file for the runner to process,
    // make this semi-synchronous to avoid choking the mailbox
    // Backlogs of over 90,000 items were observed in self-test
    // which failed to drain during the ProcessExit grace period
    // when sending only async messages.
    let message = SequencePoint (moduleId, hitPointId, track)
    lock (buffer) (fun () ->
    buffer.Add message
    if buffer.Count > Capacity then
      System.Diagnostics.Debug.WriteLine "writing"
      UnbufferedVisit f
      buffer.Clear()
      if Capacity < 1000 then Capacity <- (Capacity + 1) * 2 - 1
      )

  let Visit moduleId hitPointId =
    if Recording && mailboxOK then
     System.Diagnostics.Debug.WriteLine "visiting"
     VisitSelection (fun () -> trace.IsConnected() || Backlog() > 0)
                     (PayloadSelector IsOpenCoverRunner) moduleId hitPointId


  let ClosedownBegun = "Closedown begun"  |> GetResource |> Option.get

  let internal FlushCounter (finish:Close) _ =
   System.Diagnostics.Debug.WriteLine ("Flush Counter entry " + finish.ToString())
   if mailboxOK then
       System.Diagnostics.Debug.WriteLine "mailbox OK"
       Recording <- finish = Resume
       lock (buffer) (fun () ->
       if not Recording then UnbufferedVisit (fun _ -> true)
       buffer.Clear())
       System.Diagnostics.Debug.WriteLine "matching"
       match finish with
       | Pause
       | Resume -> System.Diagnostics.Debug.WriteLine "pause/resume"
                   mailbox.TryPostAndReply ((fun c -> Finish (finish, c)), 2000) |> ignore
       | _ -> System.Diagnostics.Debug.WriteLine "closing down"
              closedown <- true
              mailbox.TryPostAndReply ((fun c -> Finish (finish, c)), 0) |> ignore
              let rec DrainVisits () =
                try
                  let mutable msg : Message option = None
                  let a = mailbox.Receive 0
                  async {
                    let! a' = a
                    msg <- Some a'
                  } |> Async.RunSynchronously
                  if Option.isSome msg && msg |> Option.get |> DispatchVisit  then
                    DrainVisits()
                with
                  | :? TimeoutException as x -> ()
              DrainVisits()
              let m = String.Format(System.Globalization.CultureInfo.InvariantCulture,
                            ClosedownBegun, finish.ToString())
              m |> System.Diagnostics.Debug.WriteLine
              // printfn "Coverage complete"

  let internal AddErrorHandler (box:MailboxProcessor<'a>) =
    box.Error.Add MailboxError

  let internal SetErrorAction () =
    ErrorAction <- DisplayError

  let internal RunMailbox () =
    Recording <- true
    Assist.SafeDispose mailbox
    mailbox <- MakeMailbox ()
    mailboxOK <- true
    AddErrorHandler mailbox
    SetErrorAction ()
    mailbox.Start()

  // Register event handling
  let DoPause =
    FlushCounter Pause

  let DoResume =
    FlushCounter Resume

  let internal StartWatcher() =
     Watcher.Path <- Path.GetDirectoryName <| SignalFile()
     Watcher.Filter <- Path.GetFileName <| SignalFile()
     Watcher.Created.Add DoResume
     Watcher.Deleted.Add DoPause
     Watcher.EnableRaisingEvents <- Watcher.Path |> String.IsNullOrEmpty |> not

  do
    System.Diagnostics.Debug.WriteLine "doing setup"
    AppDomain.CurrentDomain.DomainUnload.Add(FlushCounter DomainUnload)
    AppDomain.CurrentDomain.ProcessExit.Add(FlushCounter ProcessExit)
    StartWatcher ()
    InitialiseTrace ()
    RunMailbox ()
    System.Diagnostics.Debug.WriteLine "done setup "
    async {
        System.Diagnostics.Debug.WriteLine "Waiting"
        if trace.IsConnected() |> not then latch.Set() |> ignore
        latch.WaitOne() |> ignore
        System.Diagnostics.Debug.WriteLine "Released"
    } |> Async.Start