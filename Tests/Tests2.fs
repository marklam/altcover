namespace Tests

open System
open System.IO
open System.Reflection

open AltCover
open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Rocks
open Mono.Options
open NUnit.Framework
open Swensen.Unquote

#if NETCOREAPP2_1
open Microsoft.VisualStudio.TestTools.UnitTesting
#endif

[<TestFixture;TestClass>]
module AltCoverTests2 =
#if NETCOREAPP2_0
    let sample1 = "Sample1.dll"
    let monoSample1 = "../_Mono/Sample1"
#else
    let sample1 = "Sample1.exe"
#endif
    let recorderSnk = typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
                      |> Seq.find (fun n -> n.EndsWith(".Recorder.snk", StringComparison.Ordinal))

    let test' x message =
      try
        test x
      with
      | fail -> AssertionFailedException(message + Environment.NewLine + fail.Message, fail) |> raise

    let infrastructureSnk =
      Assembly.GetExecutingAssembly().GetManifestResourceNames()
      |> Seq.find (fun n -> n.EndsWith("Infrastructure.snk", StringComparison.Ordinal))

    let private ProvideKeyPair() =
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      StrongNameKeyData.Make(buffer.ToArray())

    // Instrument.fs
    [<Test;TestMethod>]
    let ShouldBeAbleToGetTheVisitReportMethod() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let recorder = AltCover.Instrument.RecordingMethod def
      recorder
      |> List.zip
           [ "System.Void AltCover.Recorder.Instance.Visit(System.String,System.Int32)";
             "System.Void AltCover.Recorder.Instance.Push(System.Int32)";
             "System.Void AltCover.Recorder.Instance.Pop()" ]
      |> List.iter (fun (n, m) -> test <@ Naming.FullMethodName m = n @>)

    [<Test;TestMethod>]
    let ShouldBeAbleToClearTheStrongNameKey() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path

      NUnit.Framework.Assert.That (def.Name.HasPublicKey)
      let key0 = def.Name.PublicKey
      NUnit.Framework.Assert.That (key0, Is.Not.Empty)
      let token0 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That (token0, Is.Not.Empty)

      AltCover.Instrument.UpdateStrongNaming def None
      NUnit.Framework.Assert.That(def.Name.HasPublicKey, Is.False)
      let key1 = def.Name.PublicKey
      NUnit.Framework.Assert.That(key1, Is.Empty)
      let token1 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That(token1, Is.Empty)

    [<Test;TestMethod>]
    let ShouldBeAbleToUpdateTheStrongNameKeyWherePossible() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let key0 = def.Name.PublicKey
      let token0 = def.Name.PublicKeyToken

      NUnit.Framework.Assert.That (def.Name.HasPublicKey)
      NUnit.Framework.Assert.That (key0, Is.Not.Empty)
      NUnit.Framework.Assert.That (token0, Is.Not.Empty)

      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      let key = StrongNameKeyData.Make(buffer.ToArray())
      AltCover.Instrument.UpdateStrongNaming def (Some key)

      NUnit.Framework.Assert.That (def.Name.HasPublicKey)
      let key1 = def.Name.PublicKey
      NUnit.Framework.Assert.That (key1, Is.Not.Null)
      NUnit.Framework.Assert.That (key1, Is.Not.EquivalentTo(key0))
      let token1 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That (token1, Is.Not.Null)
      NUnit.Framework.Assert.That (token1, Is.Not.EquivalentTo(token0))
      let token' = String.Join(String.Empty, token1|> Seq.map (fun x -> x.ToString("x2")))
      NUnit.Framework.Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))

    [<Test;TestMethod>]
    let NoKnownKeyInEmptyIndex() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownKey def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let KnownKeyMatchedInIndex() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProvideKeyPair() |> Visitor.Add
        NUnit.Framework.Assert.That (Option.isSome(Instrument.KnownKey def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let ThirdPartyKeyNotMatchedInIndex() =
      try
        Visitor.keys.Clear()
        let path = typeof<System.IO.FileAccess>.Assembly.Location
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProvideKeyPair() |> Visitor.Add
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownKey def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let FakedUpKeyIsMatchedInIndex() =
      try
        Visitor.keys.Clear()
        let path =
          typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let key = KeyStore.ArrayToIndex def.Name.PublicKey
        Visitor.keys.Add(key,
                         { Pair = StrongNameKeyData.Empty()
                           Token = [] })
        NUnit.Framework.Assert.That(Option.isSome (Instrument.KnownKey def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let NoKnownKeyIfAssemblyHasNone() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        AltCover.Instrument.UpdateStrongNaming def None
        ProvideKeyPair() |> Visitor.Add
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownKey def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let NoKnownTokenInEmptyIndex() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownToken def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let KnownTokenMatchedInIndex() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProvideKeyPair() |> Visitor.Add
        NUnit.Framework.Assert.That (Option.isSome(Instrument.KnownToken def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let NoKnownTokenIfAssemblyHasNone() =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        AltCover.Instrument.UpdateStrongNaming def None
        ProvideKeyPair() |> Visitor.Add
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownToken def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let ForeignTokenIsNotMatchedInIndex() =
      try
        Visitor.keys.Clear()
        ProvideKeyPair() |> Visitor.Add
        let path = typeof<System.IO.FileAccess>.Assembly.Location
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let key = KeyStore.ArrayToIndex def.Name.PublicKey
        NUnit.Framework.Assert.That(Option.isNone (Instrument.KnownToken def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let FakedUpTokenIsMatchedInIndex() =
      try
        Visitor.keys.Clear()
        let path =
          typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        let key = KeyStore.ArrayToIndex def.Name.PublicKey
        Visitor.keys.Add(key,
                         { Pair = StrongNameKeyData.Empty()
                           Token = [] })
        NUnit.Framework.Assert.That(Option.isSome (Instrument.KnownToken def.Name))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let GuardShouldDisposeRecordingAssemblyOnException() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let prepared = AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols prepared
      let bang = fun () -> InvalidOperationException("Bang") |> raise
      NUnit.Framework.Assert.Throws<InvalidOperationException>
        (fun () -> Instrument.Guard prepared bang |> ignore) |> ignore
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      try
        NUnit.Framework.Assert.Throws<ArgumentException>
          (fun () -> Instrument.WriteAssembly prepared outputdll) |> ignore
      finally
        Directory.EnumerateFiles
          (Path.GetDirectoryName output, (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f ->
             try
               File.Delete f
             with // occasionally the dll file is locked by another process
                  :? System.UnauthorizedAccessException | :? IOException -> ())

    [<Test;TestMethod>]
    let ShouldBeAbleToTellAnAssembly() =
      let where = Assembly.GetExecutingAssembly().Location
      let here = Path.GetDirectoryName where
      let pdb = Directory.GetFiles(here, "*.pdb")
      NUnit.Framework.Assert.That(pdb, Is.Not.Empty, "no pdb")
      pdb
      |> Seq.iter (fun p ->
           let a = CommandLine.FindAssemblyName p
           NUnit.Framework.Assert.That(String.IsNullOrWhiteSpace a, p))
      let dll = Directory.GetFiles(here, "*.dll")
      NUnit.Framework.Assert.That(dll, Is.Not.Empty, "no dll")
      dll
      |> Seq.iter (fun d ->
           let a = CommandLine.FindAssemblyName d
           NUnit.Framework.Assert.That(a
                       |> String.IsNullOrWhiteSpace
                       |> not, d))

    [<Test;TestMethod>]
    let ShouldBeAbleToValidateAnAssembly() =
      let where = Assembly.GetExecutingAssembly().Location
      let here = Path.GetDirectoryName where
      let pdb = Directory.GetFiles(here, "*.pdb")
      NUnit.Framework.Assert.That(pdb, Is.Not.Empty, "no pdb")
      CommandLine.error <- []
      pdb
      |> Seq.iter (fun p ->
           let (a, b) = CommandLine.ValidateAssembly "*" p
           NUnit.Framework.Assert.That(String.IsNullOrWhiteSpace a, p)
           NUnit.Framework.Assert.That(b |> not))
      NUnit.Framework.Assert.That(CommandLine.error.Length, Is.EqualTo pdb.Length, "pdb length")
      CommandLine.error <- []
      let dll = Directory.GetFiles(here, "*.dll")
      NUnit.Framework.Assert.That(dll, Is.Not.Empty, "no dll")
      dll
      |> Seq.iter (fun d ->
           let (a, b) = CommandLine.ValidateAssembly "*" d
           NUnit.Framework.Assert.That(a
                       |> String.IsNullOrWhiteSpace
                       |> not, d)
           NUnit.Framework.Assert.That(b))
      NUnit.Framework.Assert.That(CommandLine.error |> List.isEmpty)
      let x = CommandLine.ValidateAssembly "*" "**"
      NUnit.Framework.Assert.That(x, Is.EqualTo(String.Empty, false))

    [<Test;TestMethod>]
    let ShouldBeAbleToLocateAReference() =
      let where = Assembly.GetExecutingAssembly().Location
      let here = Path.GetDirectoryName where
#if NETCOREAPP2_0
      let json = Directory.GetFiles(here, "*.json")
      test' <@ json |> Seq.isEmpty |> not @> "no json"
      json
      |> Seq.iter (fun j ->
           let a = CommandLine.FindAssemblyName j
           test' <@ String.IsNullOrWhiteSpace a @> j)
#endif
      let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly where
      Instrument.ResolutionTable.Clear()
      try
        raw.MainModule.AssemblyReferences
        |> Seq.filter
             (fun f -> f.Name.IndexOf("Mono.Cecil", StringComparison.Ordinal) >= 0)
        |> Seq.iter (fun f ->
             let resolved = Instrument.HookResolveHandler.Invoke(null, f)
             test' <@ resolved |> isNull |> not @> <| f.ToString())
        raw.MainModule.AssemblyReferences
        |> Seq.filter
             (fun f -> f.Name.IndexOf("Mono.Cecil", StringComparison.Ordinal) >= 0)
        |> Seq.iter (fun f ->
             f.Version <- System.Version("666.666.666.666")
             let resolved = Instrument.HookResolveHandler.Invoke(null, f)
             test' <@ resolved |> isNull @> <| f.ToString())
        let found = Instrument.ResolutionTable.Keys |> Seq.toList
        found
        |> Seq.iter (fun k ->
             let matched = Instrument.ResolutionTable.[k]
             let k2 = AssemblyNameReference.Parse(k.ToString())
             k2.Version <- System.Version("666.666.666.666")
             Instrument.ResolutionTable.[k2.ToString()] <- matched)
        raw.MainModule.AssemblyReferences
        |> Seq.filter
             (fun f -> f.Name.IndexOf("Mono.Cecil", StringComparison.Ordinal) >= 0)
        |> Seq.iter (fun f ->
             f.Version <- System.Version("666.666.666.666")
             let resolved = Instrument.HookResolveHandler.Invoke(null, f)
             test' <@ resolved |> isNull |> not @> <| f.ToString())
      finally
        Instrument.ResolutionTable.Clear()

    [<Test;TestMethod>]
    let ShouldBeAbleToPrepareTheAssembly() =
      try
        Visitor.keys.Clear()
        Main.init()
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let prepared = Instrument.PrepareAssembly path
        let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProgramDatabase.ReadSymbols raw
        NUnit.Framework.Assert.That(prepared.Name.Name, Is.EqualTo(raw.Name.Name + ".g"))
        NUnit.Framework.Assert.That (prepared.Name.HasPublicKey)
        NUnit.Framework.Assert.That (prepared.Name.PublicKey, Is.Not.EquivalentTo(raw.Name.PublicKey))
        let token' = String.Join(String.Empty, prepared.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
        NUnit.Framework.Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))
        let before =
          raw.MainModule.GetTypes()
          |> Seq.filter (fun t -> t.Name = "Class4")
          |> Seq.toList
        NUnit.Framework.Assert.That(before.Length = 1)
        let before' =
          before.[0].Methods
          |> Seq.filter (fun t -> t.Name = "get_ReportFile")
          |> Seq.toList
        NUnit.Framework.Assert.That(before'.Length = 1)
        let after =
          prepared.MainModule.GetTypes()
          |> Seq.filter (fun t -> t.Name = "Class4")
          |> Seq.toList
        NUnit.Framework.Assert.That(after.Length = 1)
        let after' =
          after.[0].Methods
          |> Seq.filter (fun t -> t.Name = "get_ReportFile")
          |> Seq.toList
        NUnit.Framework.Assert.That(after'.Length = 1)
        NUnit.Framework.Assert.That(after'.[0].Body.Instructions.Count, Is.EqualTo(2))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let ShouldGetTrackingStyleIfSet() =
      let save2 = Visitor.reportFormat
      let save3 = Visitor.interval
      Visitor.TrackingNames.Clear()
      try
        Visitor.reportFormat <- Some AltCover.Base.ReportFormat.OpenCover
        Visitor.interval <- Some 1234567890
        NUnit.Framework.Assert.That
          (Visitor.ReportFormat(),
           Is.EqualTo AltCover.Base.ReportFormat.OpenCoverWithTracking)
        Visitor.interval <- None
        Visitor.TrackingNames.Add("dummy")
        NUnit.Framework.Assert.That
          (Visitor.ReportFormat(),
           Is.EqualTo AltCover.Base.ReportFormat.OpenCoverWithTracking)
        Visitor.TrackingNames.Clear()
        NUnit.Framework.Assert.That
          (Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
        Visitor.reportFormat <- Some AltCover.Base.ReportFormat.NCover
        Visitor.interval <- Some 1234567890
        NUnit.Framework.Assert.That(Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
        Visitor.interval <- None
        Visitor.TrackingNames.Add("dummy")
        NUnit.Framework.Assert.That(Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
        Visitor.TrackingNames.Clear()
        NUnit.Framework.Assert.That(Visitor.ReportFormat(), Is.EqualTo AltCover.Base.ReportFormat.NCover)
      finally
        Visitor.reportFormat <- save2
        Visitor.interval <- save3
      Visitor.TrackingNames.Clear()
#if NETCOREAPP2_0
    // TODO
#else
    [<Test;TestMethod>]
    let ShouldSymbolWriterOnWindowsOnly () =
      match Instrument.CreateSymbolWriter ".pdb" true true with
      | :? Mono.Cecil.Mdb.MdbWriterProvider -> ()
      | x -> NUnit.Framework.Assert.Fail("Mono.Cecil.Mdb.MdbWriterProvider expected but got " + x.GetType().FullName)
      match Instrument.CreateSymbolWriter ".pdb" true false with
      | :? Mono.Cecil.Pdb.PdbWriterProvider -> ()
      | x -> NUnit.Framework.Assert.Fail("Mono.Cecil.Pdb.PdbWriterProvider expected but got " + x.GetType().FullName)
      match Instrument.CreateSymbolWriter ".pdb" false false with
      | null -> ()
      | x -> NUnit.Framework.Assert.Fail("null expected but got " + x.GetType().FullName)
      match Instrument.CreateSymbolWriter ".exe" true false with
      | :? Mono.Cecil.Mdb.MdbWriterProvider -> ()
      | x -> NUnit.Framework.Assert.Fail("Mono.Cecil.Mdb.MdbWriterProvider expected but got " + x.GetType().FullName)

    [<Test;TestMethod>]
    let ShouldGetNewFilePathFromPreparedAssembly () =
      try
        Visitor.keys.Clear()
        Main.init()
        let where = Assembly.GetExecutingAssembly().Location
        let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let unique = Guid.NewGuid().ToString()
        let output = Path.GetTempFileName()
        let outputdll = output + ".dll"
        let where = output |> Path.GetDirectoryName
        let what = outputdll |> Path.GetFileName
        let second = Path.Combine(where, Guid.NewGuid().ToString())
        let alter = Path.Combine (second, what)
        Directory.CreateDirectory(second) |> ignore
        let save = Visitor.reportPath
        let save2 = Visitor.reportFormat
        let save3 = Visitor.interval
        try
          Visitor.reportPath <- Some unique
          Visitor.reportFormat <- Some AltCover.Base.ReportFormat.OpenCover
          Visitor.interval <- Some 1234567890
          Visitor.single <- true
          NUnit.Framework.Assert.That(Visitor.Sampling(), Base.Sampling.Single |> int |> Is.EqualTo)
          let prepared = Instrument.PrepareAssembly path
          let traces = System.Collections.Generic.List<string>()
          Instrument.WriteAssemblies prepared what [where;second] (fun s -> s.Replace("\r", String.Empty).Replace("\n", String.Empty) |> traces.Add)
          let expectedTraces = [
            "    " + outputdll + "                <=  Sample3.g, Version=0.0.0.0, Culture=neutral, PublicKeyToken=4ebffcaabf10ce6a"
            "    " + alter + "                <=  Sample3.g, Version=0.0.0.0, Culture=neutral, PublicKeyToken=4ebffcaabf10ce6a"
          ]
          NUnit.Framework.Assert.That(traces, Is.EquivalentTo expectedTraces)
          let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
          let isWindows =
#if NETCOREAPP2_0
                          true
#else
                          System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
          if isWindows then NUnit.Framework.Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
          let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
          let raw2 = Mono.Cecil.AssemblyDefinition.ReadAssembly alter
          NUnit.Framework.Assert.That (raw.MainModule.Mvid, Is.EqualTo raw2.MainModule.Mvid)
          NUnit.Framework.Assert.That raw.Name.HasPublicKey
          // NUnit.Framework.Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
          let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
          NUnit.Framework.Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))
          let setup = AppDomainSetup()
          setup.ApplicationBase <- Path.GetDirectoryName(where)
          let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
          try
            let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
            proxyObject.InstantiateObject(outputdll,"Sample3.Class3+Class4",[||])
            let report = proxyObject.InvokeMethod("get_ReportFile",[||]).ToString()
            NUnit.Framework.Assert.That (report, Is.EqualTo (Path.GetFullPath unique))
            let report2 = proxyObject.InvokeMethod("get_CoverageFormat",[||]) :?> System.Int32
            NUnit.Framework.Assert.That (report2, AltCover.Base.ReportFormat.OpenCoverWithTracking |> int |> Is.EqualTo)
            let report3 = proxyObject.InvokeMethod("get_Timer",[||]) :?> System.Int64
            NUnit.Framework.Assert.That (report3, 1234567890L |> Is.EqualTo)
            let report4 = proxyObject.InvokeMethod("get_Sample",[||]) :?> System.Int32
            NUnit.Framework.Assert.That (report4, AltCover.Base.Sampling.Single |> int |> Is.EqualTo)
          finally
            AppDomain.Unload(ad)
        finally
          Visitor.single <- false
          Visitor.reportPath <- save
          Visitor.reportFormat <- save2
          Visitor.interval <- save3
          Directory.EnumerateFiles(Path.GetDirectoryName output,
                                   (Path.GetFileNameWithoutExtension output) + ".*")
          |> Seq.iter (fun f -> try File.Delete f
                                with // occasionally the dll file is locked by another process
                                | :? System.UnauthorizedAccessException
                                | :? IOException -> ())
          Directory.EnumerateFiles(Path.GetDirectoryName alter,
                                   (Path.GetFileNameWithoutExtension alter) + ".*")
          |> Seq.iter (fun f -> try File.Delete f
                                with // occasionally the dll file is locked by another process
                                | :? System.UnauthorizedAccessException
                                | :? IOException -> ())

          NUnit.Framework.Assert.That(Visitor.Sampling(), Base.Sampling.All |> int |> Is.EqualTo)
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let ShouldWriteMonoAssemblyOK () =
      try
        Visitor.keys.Clear()
        Main.init()
        let where = Assembly.GetExecutingAssembly().Location
        let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample3/Sample3.dll")
        let unique = Guid.NewGuid().ToString()
        let output = Path.GetTempFileName()
        let outputdll = output + ".dll"
        let save = Visitor.reportPath
        use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
        use buffer = new MemoryStream()
        stream.CopyTo(buffer)
        let key = StrongNameKeyData.Make(buffer.ToArray())
        Visitor.defaultStrongNameKey <- Some key
        Visitor.Add key
        try
          Visitor.reportPath <- Some unique
          let prepared = Instrument.PrepareAssembly path
          Instrument.WriteAssembly prepared outputdll
// TODO -- see Instrument.WriteAssembly       NUnit.Framework.Assert.That (File.Exists (outputdll + ".mdb"))
          let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
          NUnit.Framework.Assert.That raw.Name.HasPublicKey
          // NUnit.Framework.Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
          let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
          NUnit.Framework.Assert.That (token', Is.EqualTo("4ebffcaabf10ce6a"))
          let setup = AppDomainSetup()
          setup.ApplicationBase <- Path.GetDirectoryName(where)
          let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
          try
            let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
            proxyObject.InstantiateObject(outputdll,"Sample3.Class3+Class4",[||])
            let report = proxyObject.InvokeMethod("get_ReportFile",[||]).ToString()
            NUnit.Framework.Assert.That (report, Is.EqualTo (Path.GetFullPath unique))
          finally
            AppDomain.Unload(ad)
        finally
          Visitor.reportPath <- save
          Directory.EnumerateFiles(Path.GetDirectoryName output,
                                   (Path.GetFileNameWithoutExtension output) + ".*")
          |> Seq.iter (fun f -> try File.Delete f
                                with // occasionally the mdb file is locked by another process
                                | :? IOException -> ())
      finally
        Visitor.keys.Clear()
        Visitor.defaultStrongNameKey <- None

    [<Test;TestMethod>]
    let ShouldGetVisitFromWrittenAssembly () =
      try
        Visitor.keys.Clear()
        let where = Assembly.GetExecutingAssembly().Location
        let pdb = Path.ChangeExtension(where, ".pdb")
        let path = Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
        let unique = Guid.NewGuid().ToString()
        let output = Path.GetTempFileName()
        let outputdll = output + ".dll"
        let save = Visitor.reportPath
        try
          let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
          ProgramDatabase.ReadSymbols def
          let clazz = def.MainModule.GetType("Sample3.Class1")
          let func = clazz.GetMethods() |> Seq.find (fun x -> x.Name = "get_Property")
          let clazz' = def.MainModule.GetType("Sample3.Class3")
          let func' = clazz'.GetMethods() |> Seq.find (fun x -> x.Name = "Log")
          let newValue = Instrument.InsertVisit (func.Body.Instructions.[0]) (func.Body.GetILProcessor()) func' unique 42
          NUnit.Framework.Assert.That (newValue.Operand, Is.EqualTo unique)
          NUnit.Framework.Assert.That (newValue.OpCode, Is.EqualTo OpCodes.Ldstr)
          Instrument.WriteAssembly def outputdll
          let expectedSymbols = if "Mono.Runtime" |> Type.GetType |> isNull |> not then ".dll.mdb" else ".pdb"
          let isWindows =
#if NETCOREAPP2_0
                          true
#else
                          System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
          if isWindows then NUnit.Framework.Assert.That (File.Exists (outputdll.Replace(".dll", expectedSymbols)))
          let raw = Mono.Cecil.AssemblyDefinition.ReadAssembly outputdll
          NUnit.Framework.Assert.That raw.Name.HasPublicKey
          // NUnit.Framework.Assert.That (Option.isSome <| Instrument.KnownKey raw.Name) <- not needed
          let token' = String.Join(String.Empty, raw.Name.PublicKeyToken|> Seq.map (fun x -> x.ToString("x2")))
          NUnit.Framework.Assert.That (token', Is.EqualTo("c02b1a9f5b7cade8"))
          if File.Exists(pdb) then
            // doesnt' seem to work on Mono
            let setup = AppDomainSetup()
            setup.ApplicationBase <- Path.GetDirectoryName(where)
            let ad = AppDomain.CreateDomain("ShouldGetNewFilePathFromPreparedAssembly", null, setup)
            try
              let proxyObject = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
              proxyObject.InstantiateObject(outputdll,"Sample3.Class1",[||])
              let setting = proxyObject.InvokeMethod("set_Property",[| 17 |])
              NUnit.Framework.Assert.That (setting, Is.Null)
              let getting = proxyObject.InvokeMethod("get_Property",[||]) :?> int
              NUnit.Framework.Assert.That (getting, Is.EqualTo 17)
              let isWindows =
#if NETCOREAPP2_0
                              true
#else
                              System.Environment.GetEnvironmentVariable("OS") = "Windows_NT"
#endif
              let proxyObject' = ad.CreateInstanceFromAndUnwrap(typeof<ProxyObject>.Assembly.Location,"Tests.ProxyObject") :?> ProxyObject
              proxyObject'.InstantiateObject(outputdll,"Sample3.Class3",[||])
              let log = proxyObject'.InvokeMethod("get_Visits",[||]) :?> seq<Tuple<string, int>>
              if isWindows then // HACK HACK HACK
                NUnit.Framework.Assert.That (log, Is.EquivalentTo[(unique, 42)])
            finally
              AppDomain.Unload(ad)
        finally
          Visitor.reportPath <- save
          Directory.EnumerateFiles(Path.GetDirectoryName output,
                                   (Path.GetFileNameWithoutExtension output) + ".*")
          |> Seq.iter (fun f -> try File.Delete f
                                with // occasionally the dll file is locked by another process
                                | :? System.UnauthorizedAccessException
                                | :? IOException -> ())
      finally
        Visitor.keys.Clear()
  #endif

    [<Test;TestMethod>]
    let ShouldUpdateHandlerOK([<Range(0, 31)>] selection) =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), sample1)
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let program = def.MainModule.GetType("TouchTest.Program")
      let main = program.GetMethods() |> Seq.find (fun x -> x.Name = "Main")
      let oldValue = main.Body.Instructions.[0]
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      let other = main.Body.Instructions.[1]
      let subject = Instrument.SubstituteInstruction(oldValue, newValue)
      let handler = ExceptionHandler(ExceptionHandlerType())
      handler.FilterStart <- if selection &&& 1 = 1 then oldValue
                             else other
      handler.HandlerStart <- if selection &&& 2 = 2 then oldValue
                              else other
      handler.HandlerEnd <- if selection &&& 4 = 4 then oldValue
                            else other
      handler.TryStart <- if selection &&& 8 = 8 then oldValue
                          else other
      handler.TryEnd <- if selection &&& 16 = 16 then oldValue
                        else other
      subject.SubstituteExceptionBoundary handler
      NUnit.Framework.Assert.That(handler.FilterStart,
                  Is.EqualTo(if selection &&& 1 = 1 then newValue
                             else other))
      NUnit.Framework.Assert.That(handler.HandlerStart,
                  Is.EqualTo(if selection &&& 2 = 2 then newValue
                             else other))
      NUnit.Framework.Assert.That(handler.HandlerEnd,
                  Is.EqualTo(if selection &&& 4 = 4 then newValue
                             else other))
      NUnit.Framework.Assert.That(handler.TryStart,
                  Is.EqualTo(if selection &&& 8 = 8 then newValue
                             else other))
      NUnit.Framework.Assert.That(handler.TryEnd,
                  Is.EqualTo(if selection &&& 16 = 16 then newValue
                             else other))

    [<Test;TestMethod>]
    let ShouldSubstituteInstructionOperand() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      main.Body.Instructions
      |> Seq.filter (fun i ->
           match i.Operand with
           | :? Instruction -> true
           | _ -> false)
      |> Seq.iter (fun i ->
           let subject =
             Instrument.SubstituteInstruction(i.Operand :?> Instruction, newValue)
           subject.SubstituteInstructionOperand i
           NUnit.Framework.Assert.That(i.Operand, Is.EqualTo newValue))

    [<Test;TestMethod>]
    let ShouldNotSubstituteDifferentInstructionOperand() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      main.Body.Instructions
      |> Seq.filter (fun i ->
           match i.Operand with
           | :? Instruction -> true
           | _ -> false)
      |> Seq.iter (fun i ->
           let subject = Instrument.SubstituteInstruction(i, newValue)
           let before = i.Operand
           subject.SubstituteInstructionOperand i
           NUnit.Framework.Assert.That(i.Operand, Is.SameAs before))

    // work around weird compiler error with array indexing
    let private AsIArray (x : obj) (i : int) =
      (x :?> Instruction [])
      |> Seq.mapi (fun index instr -> (index, instr))
      |> Seq.filter (fun (x, y) -> x = i)
      |> Seq.map snd
      |> Seq.head

    [<Test;TestMethod>]
    let ShouldSubstituteIntoInstructionOperandArray() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      main.Body.Instructions
      |> Seq.filter (fun i ->
           match i.Operand with
           | :? (Instruction array) -> true
           | _ -> false)
      |> Seq.collect
           (fun i -> i.Operand :?> Instruction [] |> Seq.mapi (fun o t -> (i, o, t)))
      |> Seq.iter (fun (i, o, t) ->
           let subject = Instrument.SubstituteInstruction(t, newValue)
           NUnit.Framework.Assert.That(AsIArray i.Operand o, (Is.SameAs t))
           NUnit.Framework.Assert.That(t, Is.Not.EqualTo newValue)
           subject.SubstituteInstructionOperand i
           let t' = AsIArray i.Operand
           NUnit.Framework.Assert.That(t' o, Is.EqualTo newValue))

    [<Test;TestMethod>]
    let ShouldNotSubstituteOutsideInstructionOperandArray() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      main.Body.Instructions
      |> Seq.filter (fun i ->
           match i.Operand with
           | :? (Instruction array) -> true
           | _ -> false)
      |> Seq.iter
           (fun i ->
           let subject = Instrument.SubstituteInstruction(i, newValue)
           let before = (i.Operand :?> Instruction []) |> Seq.toList
           subject.SubstituteInstructionOperand i
           Seq.zip (i.Operand :?> Instruction []) before
           |> Seq.iter (fun (after, before) -> NUnit.Framework.Assert.That(after, Is.SameAs before)))

    [<Test;TestMethod>]
    let ShouldNotSubstituteOtherOperand() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let proc = main.Body.GetILProcessor()
      let newValue = proc.Create(OpCodes.Ldc_I4, 23)
      main.Body.Instructions
      |> Seq.filter (fun i ->
           match i.Operand with
           | :? Instruction
           | :? (Instruction array) -> false
           | _ -> true)
      |> Seq.collect
           (fun i -> main.Body.Instructions |> Seq.map (fun other -> (i, other)))
      |> Seq.iter (fun (i, other) ->
           let subject = Instrument.SubstituteInstruction(other, newValue)
           let before = i.Operand
           subject.SubstituteInstructionOperand i
           NUnit.Framework.Assert.That(i.Operand, Is.SameAs before))

    [<Test;TestMethod>]
    let ShouldBeAbleToTrackAMethod() =
      let where = Assembly.GetExecutingAssembly().Location
#if NETCOREAPP2_0
      let shift = String.Empty
#else
      let shift = "/netcoreapp3.0"
#endif
      let path =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack() + shift,
           "AltCover.Recorder.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let recorder = AltCover.Instrument.RecordingMethod def
      let raw = AltCover.InstrumentContext.Build([])

      let state =
        { raw with RecordingMethodRef =
                     { raw.RecordingMethodRef with Visit = null
                                                   Push = recorder.[1]
                                                   Pop = recorder.[2] } }

      let countBefore = recorder.Head.Body.Instructions.Count

      let tailsBefore =
        recorder.Head.Body.Instructions
        |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
        |> Seq.length

      let handlersBefore = recorder.Head.Body.ExceptionHandlers.Count
      AltCover.Instrument.Track state recorder.Head Inspect.Track <| Some(42, "hello")
      NUnit.Framework.Assert.That
        (recorder.Head.Body.Instructions.Count, Is.EqualTo(countBefore + 5 - tailsBefore))
      NUnit.Framework.Assert.That
        (recorder.Head.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    [<Test;TestMethod>]
    let ShouldBeAbleToTrackAMethodWithTailCalls() =
      let where = Assembly.GetExecutingAssembly().Location
#if NETCOREAPP2_0
      let shift = String.Empty
#else
      let shift = "/netcoreapp3.0"
#endif
      let rpath =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack() + shift,
           "AltCover.Recorder.dll")
      let res =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("TailCallSample.dl_", StringComparison.Ordinal))
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly stream
      let rdef = Mono.Cecil.AssemblyDefinition.ReadAssembly rpath
      let recorder = AltCover.Instrument.RecordingMethod rdef
      let target =
        def.MainModule.GetType("Tests.Problematic").Methods
        |> Seq.find (fun m -> m.Name = "Using FsUnit")
      let raw = AltCover.InstrumentContext.Build([])

      let state =
        { raw with RecordingMethodRef =
                     { raw.RecordingMethodRef with Visit = null
                                                   Push = recorder.[1]
                                                   Pop = recorder.[2] } }

      let countBefore = target.Body.Instructions.Count

      let tailsBefore =
        target.Body.Instructions
        |> Seq.filter (fun i -> i.OpCode = OpCodes.Tail)
        |> Seq.length

      let handlersBefore = target.Body.ExceptionHandlers.Count
      AltCover.Instrument.Track state target Inspect.Track <| Some(42, "hello")
      NUnit.Framework.Assert.That
        (target.Body.Instructions.Count, Is.EqualTo(countBefore + 5 - tailsBefore))
      NUnit.Framework.Assert.That(target.Body.ExceptionHandlers.Count, Is.EqualTo(handlersBefore + 1))

    [<Test;TestMethod>]
    let ShouldBeAbleToInstrumentASwitchForNCover() =
      let where = Assembly.GetExecutingAssembly().Location
#if NETCOREAPP2_0
      let shift = String.Empty
#else
      let shift = "/netcoreapp3.0"
#endif
      let rpath =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack() + shift,
           "AltCover.Recorder.dll")
      let res =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SwitchSample.dl_", StringComparison.Ordinal))
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(res)

      let res2 =
        Assembly.GetExecutingAssembly().GetManifestResourceNames()
        |> Seq.find (fun n -> n.EndsWith("SwitchSample.pd_", StringComparison.Ordinal))
      use stream2 =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(res2)

      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly stream
      let r = Mono.Cecil.Pdb.PdbReaderProvider()
      use rr = r.GetSymbolReader(def.MainModule, stream2)
      def.MainModule.ReadSymbols(rr)

      let rdef = Mono.Cecil.AssemblyDefinition.ReadAssembly rpath
      let recorder = AltCover.Instrument.RecordingMethod rdef
      let target =
        def.MainModule.GetType("Sample15.Class1").Methods
        |> Seq.find (fun m -> m.Name = "OpenCoverSummary")
      let raw = AltCover.InstrumentContext.Build([])

      let state =
        { raw with RecordingMethodRef =
                     { raw.RecordingMethodRef with Visit = recorder.[1]
                                                   Push = recorder.[1]
                                                   Pop = recorder.[2] } }

      let switch = target.Body.Instructions
                   |> Seq.find (fun i -> i.OpCode = OpCodes.Switch)
      let targets = switch.Operand :?> Instruction array
                    |> Array.map (fun i -> i.Offset)
      NUnit.Framework.Assert.That (targets, Is.EquivalentTo [ 31; 33; 31; 33; 31 ])

      let m = Node.Method (target, Inspect.Instrument, None)
      let steps = Visitor.BuildSequence m

      NUnit.Framework.Assert.That(steps, Is.Not.Empty)

      let visitors = [ Visitor.EncloseState Instrument.InstrumentationVisitor state ]

      steps
      |> Seq.fold Visitor.apply (visitors |> Seq.toList)
      |> ignore

      let switch2 = target.Body.Instructions
                   |> Seq.find (fun i -> i.OpCode = OpCodes.Switch)
      let targets2 = switch2.Operand :?> Instruction array
                    |> Array.map (fun i -> i.Offset)

      let next = switch2.Next.Offset
      let n2 = next + 2
      // Need to check the heisenstate here

      //case of 43
      //IL_0000: ldstr ""
      //IL_0005: ldc.i4.s 24
      //IL_0007: call System.Void AltCover.Recorder.Instance::Push(System.Int32)
      //IL_000c: ldarg.0
      //IL_000d: call System.Int32 Sample15.TeamCityFormat::get_Tag()
      //IL_0012: switch IL_002b,IL_002d,IL_002b,IL_002d,IL_002b
      //IL_002b: br.s IL_0041

      let expected = 43
      if next <> expected
      then target.Body.Instructions
           |> Seq.iter (printfn "%A")
      NUnit.Framework.Assert.That (next, Is.EqualTo expected)
      NUnit.Framework.Assert.That (targets2, Is.EquivalentTo [ next; n2; next; n2; next ])

    [<Test;TestMethod>]
    let ShouldNotChangeAnUntrackedMethod() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      let recorder = AltCover.Instrument.RecordingMethod def
      let state = AltCover.InstrumentContext.Build([])
      let countBefore = recorder.Head.Body.Instructions.Count
      let handlersBefore = recorder.Head.Body.ExceptionHandlers.Count
      AltCover.Instrument.Track state recorder.Head Inspect.Track None
      NUnit.Framework.Assert.That(recorder.Head.Body.Instructions.Count, Is.EqualTo countBefore)
      NUnit.Framework.Assert.That(recorder.Head.Body.ExceptionHandlers.Count, Is.EqualTo handlersBefore)

    [<Test;TestMethod>]
    let SwitchBranchesShouldInstrumentByPushingDown() =
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let method =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.find (fun m -> m.Name = "as_bar")
      Visitor.Visit [] [] // cheat reset
      try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        let branches =
          Visitor.Deeper <| Node.Method(method, Inspect.Instrument, None)
          |> Seq.map (fun n ->
               match n with
               | BranchPoint b -> Some b
               | _ -> None)
          |> Seq.choose id
          |> Seq.take 2 // start of a switch
          |> Seq.toList
        match branches with
        | [ b1; b2 ] ->
          NUnit.Framework.Assert.That(b1.Start.OpCode, Is.EqualTo OpCodes.Switch)
          NUnit.Framework.Assert.That(b2.Start.OpCode, Is.EqualTo OpCodes.Switch)
          NUnit.Framework.Assert.That(b1.Start.Offset, Is.EqualTo b2.Start.Offset)
        | _ -> NUnit.Framework.Assert.Fail("wrong number of items")
        let raw = AltCover.InstrumentContext.Build([])

        let state =
          { raw with RecordingMethodRef =
                       { raw.RecordingMethodRef with Visit = method
                                                     Push = null
                                                     Pop = null }
                     MethodWorker = method.Body.GetILProcessor() }

        let next = branches.Head.Start.Next
        branches |> Seq.iter (fun b -> Instrument.VisitBranchPoint state b |> ignore)
        let inject =
          Seq.unfold (fun (state : Cil.Instruction) ->
            if isNull state || state = next then None
            else Some(state, state.Next)) branches.Head.Start
          |> Seq.skip 1
          |> Seq.toList
        NUnit.Framework.Assert.That(inject.Length, Is.EqualTo 8)
        let switches = branches.Head.Start.Operand :?> Instruction [] |> Seq.toList
        NUnit.Framework.Assert.That(switches.[0], Is.EqualTo inject.[1])
        NUnit.Framework.Assert.That(switches.[1], Is.EqualTo inject.[0])
        NUnit.Framework.Assert.That(inject.[0].Operand, Is.EqualTo inject.[5])
        NUnit.Framework.Assert.That
          ((inject.[2].Operand :?> int) &&& Base.Counter.BranchMask, Is.EqualTo 1)
        NUnit.Framework.Assert.That
          ((inject.[6].Operand :?> int) &&& Base.Counter.BranchMask, Is.EqualTo 0)
      finally
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- None

    [<Test;TestMethod>]
    let PseudoSwitchVisibleBranchesShouldSkipNonRepresentativeCases() =
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where), "Sample16.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      Visitor.coalesceBranches := true
      let method =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.find (fun m -> m.Name = "Bar")
      Visitor.Visit [] [] // cheat reset
      try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        let branches =
          Visitor.Deeper <| Node.Method(method, Inspect.Instrument, None)
          |> Seq.map (fun n ->
               match n with
               | BranchPoint b -> Some b
               | _ -> None)
          |> Seq.choose id
          |> Seq.skip 2
          |> Seq.take 2 // first of "switch"
          |> Seq.toList
        match branches with
        | [ b1; b2 ] ->
          NUnit.Framework.Assert.That(b1.Start.OpCode, Is.EqualTo OpCodes.Brfalse_S)
          NUnit.Framework.Assert.That(b2.Start.OpCode, Is.EqualTo OpCodes.Brfalse_S)

          NUnit.Framework.Assert.That(b1.Start.Offset, Is.EqualTo b2.Start.Offset)
        | _ -> NUnit.Framework.Assert.Fail("wrong number of items")
        let raw = AltCover.InstrumentContext.Build([])

        let state =
          { raw with RecordingMethodRef =
                       { raw.RecordingMethodRef with Visit = method
                                                     Push = null
                                                     Pop = null }
                     MethodWorker = method.Body.GetILProcessor() }

        let next = branches.Head.Start.Next
        branches |> Seq.iter (fun b -> Instrument.VisitBranchPoint state b |> ignore)
        let inject =
          Seq.unfold (fun (state : Cil.Instruction) ->
            if isNull state || state = next then None
            else Some(state, state.Next)) branches.Head.Start
          |> Seq.skip 1
          |> Seq.toList
        NUnit.Framework.Assert.That(inject.Length, Is.EqualTo 5)
        let jump = branches.Head.Start.Operand :?> Instruction
        NUnit.Framework.Assert.That(jump, Is.EqualTo inject.[1])
        NUnit.Framework.Assert.That(inject.[0].Operand, Is.EqualTo inject.[4].Next)
        NUnit.Framework.Assert.That
          ((inject.[2].Operand :?> int) &&& Base.Counter.BranchMask, Is.EqualTo branches.[1].Uid)
      finally
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- None
        Visitor.coalesceBranches := false

    [<Test;TestMethod>]
    let SimpleBranchShouldInstrumentByPushingDown() =
      let where = Assembly.GetExecutingAssembly().Location
      let path = Path.Combine(Path.GetDirectoryName(where),
#if NETCOREAPP2_0
                    "Sample1.dll")
#else
                    "Sample1.exe")
#endif

      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let method =
        def.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.find (fun m -> m.Name = "Main")
      Visitor.Visit [] [] // cheat reset
      try
        Visitor.reportFormat <- Some Base.ReportFormat.OpenCover
        let branches =
          Visitor.Deeper <| Node.Method(method, Inspect.Instrument, None)
          |> Seq.map (fun n ->
               match n with
               | BranchPoint b -> Some b
               | _ -> None)
          |> Seq.choose id
          |> Seq.take 2 // start of a switch
          |> Seq.toList
        match branches with
        | [ b1; b2 ] -> ()
        | _ -> NUnit.Framework.Assert.Fail("wrong number of items")
        let raw = AltCover.InstrumentContext.Build([])

        let state =
          { raw with RecordingMethodRef =
                       { raw.RecordingMethodRef with Visit = method
                                                     Push = null
                                                     Pop = null }
                     MethodWorker = method.Body.GetILProcessor() }

        let next = branches.Head.Start.Next
        branches |> Seq.iter (fun b -> Instrument.VisitBranchPoint state b |> ignore)
        let inject =
          Seq.unfold (fun (state : Cil.Instruction) ->
            if isNull state || state = next then None
            else Some(state, state.Next)) branches.Head.Start
          |> Seq.skip 1
          |> Seq.toList
        NUnit.Framework.Assert.That(inject.Length, Is.EqualTo 8)
        NUnit.Framework.Assert.That(inject.[0].Operand, Is.EqualTo inject.[5])
        NUnit.Framework.Assert.That
          ((inject.[2].Operand :?> int) &&& Base.Counter.BranchMask, Is.EqualTo 1)
        NUnit.Framework.Assert.That
          ((inject.[6].Operand :?> int) &&& Base.Counter.BranchMask, Is.EqualTo 0)
      finally
        Visitor.NameFilters.Clear()
        Visitor.reportFormat <- None
#if COVERLET
#else
    [<Test;TestMethod>]
    let StartShouldLoadRecordingAssembly () =
      let def = Instrument.InstrumentationVisitor (InstrumentContext.Build []) (Start [])
      NUnit.Framework.Assert.That (def.RecordingAssembly.Name.Name, Is.EqualTo "AltCover.Recorder.g")
#endif

    [<Test;TestMethod>]
    let TypeShouldNotChangeState() =
      let input = InstrumentContext.Build []
      let output =
        Instrument.InstrumentationVisitor input (Node.Type(null, Inspect.Ignore))
      NUnit.Framework.Assert.That(output, Is.SameAs input)

    [<Test;TestMethod>]
    let ExcludedMethodShouldNotChangeState() =
      let input = InstrumentContext.Build []
      let output =
        Instrument.InstrumentationVisitor input (Node.Method(null, Inspect.Ignore, None))
      NUnit.Framework.Assert.That(output, Is.SameAs input)

    [<Test;TestMethod>]
    let IncludedMethodShouldChangeState() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
      let input = InstrumentContext.Build []
      let output =
        Instrument.InstrumentationVisitor input
          (Node.Method(func, Inspect.Instrument, None))
      NUnit.Framework.Assert.That(output.MethodBody, Is.SameAs func.Body)

    [<Test;TestMethod>]
    let ExcludedAfterMethodShouldNotChangeState() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")

      let opcodes =
        func.Body.Instructions
        |> Seq.map (fun i -> i.OpCode)
        |> Seq.toList

      let input = { InstrumentContext.Build [] with MethodBody = func.Body }
      input.MethodBody.SimplifyMacros()
      let paired = Seq.zip opcodes input.MethodBody.Instructions |> Seq.toList
      NUnit.Framework.Assert.That(paired |> Seq.exists (fun (i, j) -> i <> j.OpCode))
      let diff = paired |> List.map (fun (i, j) -> (i, i = j.OpCode))
      let output =
        Instrument.InstrumentationVisitor input
          (Node.AfterMethod(func, Inspect.Ignore, None))
      NUnit.Framework.Assert.That(output, Is.SameAs input)
      let paired' = Seq.zip diff input.MethodBody.Instructions
      NUnit.Framework.Assert.That(paired' |> Seq.forall (fun ((i, x), j) -> x = (i = j.OpCode)))

    [<Test;TestMethod>]
    let IncludedAfterMethodShouldRewriteMethod() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let module' = def.MainModule.GetType("N.DU")

      let du =
        module'.NestedTypes
        |> Seq.filter (fun t -> t.Name = "MyUnion")
        |> Seq.head

      let func = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")

      let opcodes =
        func.Body.Instructions
        |> Seq.map (fun i -> i.OpCode)
        |> Seq.toList

      let input = { InstrumentContext.Build [] with MethodBody = func.Body }
      input.MethodBody.SimplifyMacros()
      let paired = Seq.zip opcodes input.MethodBody.Instructions
      NUnit.Framework.Assert.That(paired |> Seq.exists (fun (i, j) -> i <> j.OpCode))
      let output =
        Instrument.InstrumentationVisitor input
          (Node.AfterMethod(func, Inspect.Instrument, None))
      NUnit.Framework.Assert.That(output, Is.SameAs input)
      let paired' = Seq.zip opcodes input.MethodBody.Instructions
      NUnit.Framework.Assert.That(paired' |> Seq.forall (fun (i, j) -> i = j.OpCode))

    [<Test;TestMethod>]
    let UpdateStrongReferencesShouldChangeSigningKeyWherePossible() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let token0 = def.Name.PublicKeyToken
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      Visitor.defaultStrongNameKey <- Some(StrongNameKeyData.Make(buffer.ToArray()))
      let result = Instrument.UpdateStrongReferences def []
      let token1 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That (token1, Is.Not.Null)
      NUnit.Framework.Assert.That (token1, Is.Not.EquivalentTo(token0))

      let token' =
        String.Join(String.Empty, token1 |> Seq.map (fun x -> x.ToString("x2")))
      NUnit.Framework.Assert.That (token', Is.EqualTo "4ebffcaabf10ce6a" )
      NUnit.Framework.Assert.That(result, Is.Empty)

    [<Test;TestMethod>]
    let UpdateStrongReferencesShouldRemoveSigningKeyIfRequired() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let token0 = def.Name.PublicKeyToken
      Visitor.defaultStrongNameKey <- None
      let result = Instrument.UpdateStrongReferences def [ "nunit.framework" ]
      let token1 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That(token1, Is.Empty)
      NUnit.Framework.Assert.That(token1, Is.Not.EquivalentTo(token0))
      let token' =
        String.Join(String.Empty, token1 |> Seq.map (fun x -> x.ToString("x2")))
      NUnit.Framework.Assert.That(token', Is.EqualTo String.Empty)
      NUnit.Framework.Assert.That(result.Count, Is.EqualTo 1)
      let key = result.Keys |> Seq.head
      let value = result.Values |> Seq.head
      let ptr = key.LastIndexOf("=")
      NUnit.Framework.Assert.That(key.Substring(0, ptr), Is.EqualTo(value.Substring(0, ptr)))
      NUnit.Framework.Assert.That(value.Substring(ptr), Is.EqualTo "=null")

    [<Test;TestMethod>]
    let UpdateStrongReferencesShouldNotAddASigningKey() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine
          (where.Substring(0, where.IndexOf("_Binaries")) + "_Mono/Sample1", "Sample1.exe")
#if NETCOREAPP2_0

      let path' =
        if File.Exists path then path
        else
          Path.Combine
            (where.Substring(0, where.IndexOf("_Binaries")) + monoSample1, "Sample1.exe")
#else
      let path' = path
#endif
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path'
      ProgramDatabase.ReadSymbols def
#if NETCOREAPP2_0
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      Visitor.defaultStrongNameKey <- Some(StrongNameKeyData.Make(buffer.ToArray()))
      let result = Instrument.UpdateStrongReferences def []
      let token1 = def.Name.PublicKeyToken
      NUnit.Framework.Assert.That(token1, Is.Empty)
      NUnit.Framework.Assert.That(result, Is.Empty)

    [<Test;TestMethod>]
    let UpdateStrongReferencesShouldTrackReferences() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def |> ignore
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      Visitor.defaultStrongNameKey <- Some(StrongNameKeyData.Make(buffer.ToArray()))
      let result = Instrument.UpdateStrongReferences def [ "nunit.framework"; "nonesuch" ]
      NUnit.Framework.Assert.That(result.Count, Is.EqualTo 1)

      NUnit.Framework.Assert.That (result.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
      let key = result.Keys |> Seq.head
      NUnit.Framework.Assert.That (result.Values |> Seq.head,
                   Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))

    [<Test;TestMethod>]
    let UpdateStrongReferencesShouldTrackReferencesEvenFakes() =
      try
        let where = Assembly.GetExecutingAssembly().Location
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProgramDatabase.ReadSymbols def |> ignore
        let npath = typeof<TestAttribute>.Assembly.Location
        let ndef = Mono.Cecil.AssemblyDefinition.ReadAssembly npath
        let key = KeyStore.ArrayToIndex ndef.Name.PublicKey
        use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)

        use buffer = new MemoryStream()
        stream.CopyTo(buffer)
        let ourKeyPair = StrongNameKeyData.Make(buffer.ToArray())
        Visitor.defaultStrongNameKey <- Some ourKeyPair
        Visitor.keys.Add(key,
                         { Pair = ourKeyPair
                           Token = [] })
        let result =
          Instrument.UpdateStrongReferences def [ "nunit.framework"; "nonesuch" ]
        NUnit.Framework.Assert.That(result.Count, Is.EqualTo 1)
        NUnit.Framework.Assert.That (result.Values |> Seq.head, Does.EndWith "PublicKeyToken=4ebffcaabf10ce6a")
        let key = result.Keys |> Seq.head
        NUnit.Framework.Assert.That (result.Values |> Seq.head,
                     Is.EqualTo (key.Substring(0, key.Length - 16) + "4ebffcaabf10ce6a"))
      finally
        Visitor.keys.Clear()

    [<Test;TestMethod>]
    let ExcludedAssemblyRefsAreNotUpdated() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let refs = def.MainModule.AssemblyReferences |> Seq.toList
#if NETCOREAPP2_0
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      Visitor.defaultStrongNameKey <- Some(StrongNameKeyData.Make(buffer.ToArray()))
      let fake =
        Mono.Cecil.AssemblyDefinition.ReadAssembly
          (Assembly.GetExecutingAssembly().Location)
      let state = InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]
      let visited = Node.Assembly(def, Inspect.Ignore, [])
      let result =
        Instrument.InstrumentationVisitor { state with RecordingAssembly = fake } visited
      NUnit.Framework.Assert.That(def.MainModule.AssemblyReferences, Is.EquivalentTo refs)

    [<Test;TestMethod>]
    let IncludedAssemblyRefsAreUpdated() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let refs = def.MainModule.AssemblyReferences |> Seq.toList
#if NETCOREAPP2_0
      use stream =
        Assembly.GetExecutingAssembly().GetManifestResourceStream(infrastructureSnk)
#else
      use stream = typeof<AltCover.Node>.Assembly.GetManifestResourceStream(recorderSnk)
#endif
      use buffer = new MemoryStream()
      stream.CopyTo(buffer)
      Visitor.defaultStrongNameKey <- Some(StrongNameKeyData.Make(buffer.ToArray()))
      let fake =
        Mono.Cecil.AssemblyDefinition.ReadAssembly
          (Assembly.GetExecutingAssembly().Location)
      let state = InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]
      let visited = Node.Assembly(def, Inspect.Instrument, [])
      let result =
        Instrument.InstrumentationVisitor { state with RecordingAssembly = fake } visited
      NUnit.Framework.Assert.That
        (def.MainModule.AssemblyReferences, Is.EquivalentTo(refs @ [ fake.Name ]))

    [<Test;TestMethod>]
    let ExcludedModuleJustRecordsMVid() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let visited = Node.Module(def.MainModule, Inspect.Ignore)
      let state = InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]
      let result = Instrument.InstrumentationVisitor state visited
      NUnit.Framework.Assert.That
        (result, Is.EqualTo { state with ModuleId = def.MainModule.Mvid.ToString() })

    [<Test;TestMethod>]
    let IncludedModuleEnsuresRecorder() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let visited = Node.Module(def.MainModule, Inspect.Instrument)
      let state = InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]
      let path' =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
      let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'

      let visit =
        def'.MainModule.GetAllTypes()
        |> Seq.filter (fun t -> t.FullName = "AltCover.Recorder.Instance")
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.filter (fun m -> m.Name = "Visit" || m.Name = "Push" || m.Name = "Pop")
        |> Seq.sortBy (fun m -> m.Name)
        |> Seq.toList
        |> List.rev

      let state' = { state with RecordingAssembly = def' }
      let result = Instrument.InstrumentationVisitor state' visited

      test <@ result.RecordingMethodRef.Visit.Module = def.MainModule @>
      test <@ string result.RecordingMethodRef.Visit =
                 (visit
                  |> Seq.head
                  |> string) @>
      test <@ string result.RecordingMethodRef.Push =
                 (visit
                  |> Seq.skip 1
                  |> Seq.head
                  |> string) @>
      test <@ string result.RecordingMethodRef.Pop =
                 (visit
                  |> Seq.skip 2
                  |> Seq.head
                  |> string) @>
      test <@ { result with RecordingMethodRef =
                                  { Visit = null
                                    Push = null
                                    Pop = null } } =
                             { state' with ModuleId = def.MainModule.Mvid.ToString()
                                           RecordingMethod = visit
                                           RecordingMethodRef =
                                             { Visit = null
                                               Push = null
                                               Pop = null } } @>

    [<Test;TestMethod>]
    let ExcludedMethodPointIsPassThrough() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let visited = Node.MethodPoint(null, None, 0, false)
      let state = InstrumentContext.Build []
      let result = Instrument.InstrumentationVisitor state visited
      NUnit.Framework.Assert.That(result, Is.SameAs state)

    [<Test;TestMethod>]
    let IncludedMethodPointInsertsVisit() =
      let where = Assembly.GetExecutingAssembly().Location
      let pdb = Path.ChangeExtension(where, ".pdb")
      if File.Exists(pdb) then // skip when we don't have symbols on travis
        let path =
          Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
        let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
        ProgramDatabase.ReadSymbols def
        let module' = def.MainModule.GetType("N.DU")

        let du =
          module'.NestedTypes
          |> Seq.filter (fun t -> t.Name = "MyUnion")
          |> Seq.head

        let main = du.GetMethods() |> Seq.find (fun x -> x.Name = "as_bar")
        let proc = main.Body.GetILProcessor()
        let dbg = main.DebugInformation

        let target =
          main.Body.Instructions
          |> Seq.filter (dbg.GetSequencePoint
                         >> isNull
                         >> not)
          |> Seq.head

        let visited = Node.MethodPoint(target, None, 32767, true)
        NUnit.Framework.Assert.That(target.Previous, Is.Null)
        let state =
          { (InstrumentContext.Build []) with MethodWorker = proc
                                              MethodBody = main.Body
                                              RecordingMethodRef =
                                                { Visit =
                                                    def.MainModule.ImportReference main
                                                  Push = null
                                                  Pop = null } }

        let result = Instrument.InstrumentationVisitor state visited
        NUnit.Framework.Assert.That(result, Is.SameAs state)
        NUnit.Framework.Assert.That(target.Previous.OpCode, Is.EqualTo OpCodes.Call)

    [<Test;TestMethod>]
    let IncludedModuleDoesNotChangeRecorderJustTheReference() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample2.dll")
      let def = Mono.Cecil.AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols def
      let visited = Node.Module(def.MainModule, Inspect.Instrument)
      let state = InstrumentContext.Build [ "nunit.framework"; "nonesuch" ]
      let path' =
        Path.Combine
          (Path.GetDirectoryName(where) + AltCoverTests.Hack(), "AltCover.Recorder.dll")
      let def' = Mono.Cecil.AssemblyDefinition.ReadAssembly path'

      let visit =
        def'.MainModule.GetAllTypes()
        |> Seq.collect (fun t -> t.Methods)
        |> Seq.filter (fun m -> m.Name = "Visit")
        |> Seq.head

      let def'' = Mono.Cecil.AssemblyDefinition.ReadAssembly where
      let v = def''.MainModule.ImportReference visit

      let r =
        { RecorderRefs.Build() with Visit = v
                                    Push = v
                                    Pop = v }

      let state' =
        { state with RecordingAssembly = def'
                     RecordingMethod = [ visit; visit; visit ]
                     RecordingMethodRef = r }

      let result = Instrument.InstrumentationVisitor state' visited
      let ref'' = def.MainModule.ImportReference visit
      NUnit.Framework.Assert.That(result.RecordingMethodRef.Visit.Module, Is.EqualTo(def.MainModule))
      NUnit.Framework.Assert.That(string result.RecordingMethodRef, Is.EqualTo(string r))
      NUnit.Framework.Assert.That({ result with RecordingMethodRef = RecorderRefs.Build() },
                  Is.EqualTo { state' with ModuleId = def.MainModule.Mvid.ToString()
                                           RecordingMethod = [ visit; visit; visit ]
                                           RecordingMethodRef = RecorderRefs.Build() })

    [<Test;TestMethod>]
    let AfterModuleShouldNotChangeState() =
      let input = InstrumentContext.Build []
      let output = Instrument.InstrumentationVisitor input AfterModule
      NUnit.Framework.Assert.That(output, Is.SameAs input)

    [<Test;TestMethod>]
    let JSONInjectionTransformsStandaloneFileAsExpected() =
      let inputName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.json")
      let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)
      use reader = new StreamReader(stream)
      let result = Instrument.injectJSON <| reader.ReadToEnd()
      use stream' = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
      use reader' = new StreamReader(stream')
      let expected = reader'.ReadToEnd()
      let version = System.AssemblyVersionInformation.AssemblyVersion
      let transform (s : string) =
        s.Replace("\r\n", "\n")
         .Replace("AltCover.Recorder.g/1.4.0.0", "AltCover.Recorder.g/" + version)
         .Replace("AltCover.Recorder.g\": \"1.4.0.0",
                  "AltCover.Recorder.g\": \"" + version)
      NUnit.Framework.Assert.That(transform result, Is.EqualTo(transform expected))

    [<Test;TestMethod>]
    let JSONInjectionTransformsDependencyFileAsExpected() =
      let inputName = infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.json")
      let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample2.deps.after.json")
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(inputName)
      use reader = new StreamReader(stream)
      let result = Instrument.injectJSON <| reader.ReadToEnd()
      use stream' = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
      use reader' = new StreamReader(stream')
      let expected = reader'.ReadToEnd()
      let version = System.AssemblyVersionInformation.AssemblyVersion
      let transform (s : string) =
        s.Replace("\r\n", "\n")
         .Replace("AltCover.Recorder.g/2.0.0.0", "AltCover.Recorder.g/" + version)
         .Replace("AltCover.Recorder.g\": \"2.0.0.0",
                  "AltCover.Recorder.g\": \"" + version)
      NUnit.Framework.Assert.That(transform result, Is.EqualTo(transform expected))

    [<Test;TestMethod>]
    let JSONInjectionIsIdempotent() =
      let resultName = infrastructureSnk.Replace("Infrastructure.snk", "Sample1.deps.after.json")
      use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resultName)
      use reader = new StreamReader(stream)
      let expected = reader.ReadToEnd()
      let result = Instrument.injectJSON <| expected
      NUnit.Framework.Assert.That
        (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))

    [<Test;TestMethod>]
    let NonFinishShouldDisposeRecordingAssembly() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let prepared = AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols prepared
      let state = { InstrumentContext.Build [] with RecordingAssembly = prepared }
      NUnit.Framework.Assert.Throws<InvalidOperationException>
        (fun () ->
        Instrument.InstrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise) state AfterType
        |> ignore) |> ignore
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      try
        NUnit.Framework.Assert.Throws<ArgumentException>
          (fun () -> Instrument.WriteAssembly prepared outputdll) |> ignore
      finally
        Directory.EnumerateFiles
          (Path.GetDirectoryName output, (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f ->
             try
               File.Delete f
             with // occasionally the dll file is locked by another process
                  :? System.UnauthorizedAccessException | :? IOException -> ())

    [<Test;TestMethod>]
    let NonFinishShouldNotDisposeNullRecordingAssembly() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let state = { InstrumentContext.Build [] with RecordingAssembly = null }
      // Would be NullreferenceException if we tried it
      NUnit.Framework.Assert.Throws<InvalidOperationException>
        (fun () ->
        Instrument.InstrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise) state AfterType
        |> ignore) |> ignore

    [<Test;TestMethod>]
    let FinishShouldLeaveRecordingAssembly() =
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(Path.GetDirectoryName(where) + AltCoverTests.Hack(), "Sample3.dll")
      let state = { InstrumentContext.Build [] with RecordingAssembly = null }
      let prepared = AssemblyDefinition.ReadAssembly path
      ProgramDatabase.ReadSymbols prepared
      NUnit.Framework.Assert.Throws<InvalidOperationException>
        (fun () ->
        Instrument.InstrumentationVisitorWrapper
          (fun _ _ -> InvalidOperationException("Bang") |> raise) state Finish |> ignore)
      |> ignore
      let output = Path.GetTempFileName()
      let outputdll = output + ".dll"
      try
        Instrument.WriteAssembly prepared outputdll
      finally
        Directory.EnumerateFiles
          (Path.GetDirectoryName output, (Path.GetFileNameWithoutExtension output) + ".*")
        |> Seq.iter (fun f ->
             try
               File.Delete f
             with // occasionally the dll file is locked by another process
                  :? System.UnauthorizedAccessException | :? IOException -> ())

    // CommandLine.fs
    [<Test;TestMethod>]
    let StrongNameKeyCanBeValidatedExceptOnNetCore() =
      let input = Path.Combine(AltCover.SolutionRoot.location, "Build/Infrastructure.snk")
      let (pair, ok) = CommandLine.ValidateStrongNameKey "key" input
      NUnit.Framework.Assert.That(ok, Is.True, "Strong name is OK")
      NUnit.Framework.Assert.That(pair, Is.Not.Null)
      NUnit.Framework.Assert.That(pair.PublicKey, Is.Not.Null)
      NUnit.Framework.Assert.That
        (CommandLine.ValidateStrongNameKey "key" (String(Path.GetInvalidPathChars())),
         Is.EqualTo(StrongNameKeyData.Empty(), false))
      NUnit.Framework.Assert.That
        (CommandLine.ValidateStrongNameKey "key"
         <| Assembly.GetExecutingAssembly().Location, Is.EqualTo(StrongNameKeyData.Empty(), false))

    [<Test;TestMethod>]
    let OutputCanBeExercised() =
      let sink = StringSink(ignore)
      Output.SetInfo sink
      Output.SetError sink
      Output.Echo <- ignore
      Output.Usage <- ignore
      NUnit.Framework.Assert.That(Output.Usage, Is.Not.Null)
      typeof<TeamCityFormat>.Assembly.GetTypes()
      |> Seq.filter
           (fun t -> (string t = "AltCover.Output") || (string t = "AltCover.AltCover"))
      |> Seq.collect (fun t -> t.GetNestedTypes(BindingFlags.NonPublic))
      |> Seq.filter (fun t ->
           let tokens =
             [ "Info"; "Echo"; "Error"; "Usage"; "Warn"; "ToConsole"; "SetInfo";
               "SetError"; "SetWarn" ]
           let name = t.Name
           tokens |> List.exists name.StartsWith)
      |> Seq.iter (fun t ->
           let p = t.GetType().GetProperty("DeclaredConstructors")
           let c = p.GetValue(t, null) :?> ConstructorInfo []
           let c0 = c |> Seq.head
           let p = c0.GetParameters().Length

           let o =
             c0.Invoke(if p = 0 then null
                       else [| sink |])

           let invoke = t.GetMethod("Invoke")
           let param = invoke.GetParameters() |> Seq.head

           let arg : obj =
             if param.ParameterType = typeof<String> then String.Empty :> obj
             else { Intro = String.Empty; Options = OptionSet(); Options2 = OptionSet() } :> obj
           invoke.Invoke(o, [| arg |]) |> ignore)

    [<Test;TestMethod>]
    let NoThrowNoErrorLeavesAllOK() =
      try
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation ignore () true
        NUnit.Framework.Assert.That(CommandLine.error, Is.Empty)
        NUnit.Framework.Assert.That(CommandLine.exceptions, Is.Empty)
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []

    [<Test;TestMethod>]
    let NoThrowWithErrorIsSignalled() =
      try
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation
          (fun () -> CommandLine.error <- [ "NoThrowWithErrorIsSignalled" ]) () true
        NUnit.Framework.Assert.That(CommandLine.error, Is.Not.Empty)
        NUnit.Framework.Assert.That(CommandLine.exceptions, Is.Empty)
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []

    [<Test;TestMethod>]
    let ArgumentExceptionWrites() =
      let saved = (Output.Info, Output.Error)
      let err = System.Text.StringBuilder()
      let info = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
        Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
        let unique = "ArgumentException " + Guid.NewGuid().ToString()
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation (fun () ->
          let inner = InvalidOperationException()
          ArgumentException(unique, inner) |> raise) () true
        NUnit.Framework.Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
        NUnit.Framework.Assert.That
          (CommandLine.exceptions |> List.map (fun e -> e.Message),
           Is.EquivalentTo [ unique ])
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let there = Path.Combine(here, Guid.NewGuid().ToString())
        let toInfo = Directory.CreateDirectory there
        Visitor.outputDirectories.Clear()
        Visitor.inputDirectories.Clear()
        Visitor.outputDirectories.Add toInfo.FullName
        Visitor.inputDirectories.Add here
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(err.ToString(), Is.Empty)
        let name = "ArgumentExceptionWrites"
        CommandLine.logExceptionsToFile name false
        let target = Path.Combine(toInfo.FullName, name)
        let target' = Path.Combine(here, name)
        NUnit.Framework.Assert.That(File.Exists target, target)
        let lines' =
          target
          |> File.ReadAllLines
          |> Seq.toList

        let head = lines' |> List.head

        let lines = if head.Length > 80
                    then lines'
                    else let t = lines' |> List.tail
                         (head + List.head t) :: (List.tail t)

        NUnit.Framework.Assert.That
          (lines.[0],
           Is.EqualTo
             ("System.ArgumentException: " + unique
              + " ---> System.InvalidOperationException: Operation is not valid due to the current state of the object."),
            sprintf "lines = %A" lines)
        NUnit.Framework.Assert.That
          (lines.[1], Does.StartWith("   --- End of inner exception stack trace ---"))
        NUnit.Framework.Assert.That
          (lines.[2].Replace("+", ".").Trim(),
           Does.StartWith
             ("at Tests.AltCoverTests2.ArgumentExceptionWrites@"))
        NUnit.Framework.Assert.That
          (lines.[3].Trim(), Does.StartWith("at AltCover.CommandLine.doPathOperation"))
        NUnit.Framework.Assert.That(lines |> List.skip 4, Is.Not.Empty)
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That
          (err.ToString().Trim(), Is.EqualTo("Details written to " + target + "|"))
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []
        Output.Info <- (fst saved)
        Output.Error <- (snd saved)
        Visitor.outputDirectories.Clear()

    [<Test;TestMethod>]
    let ArgumentExceptionWritesEx() =
      let saved = (Output.Info, Output.Error)
      let err = System.Text.StringBuilder()
      let info = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
        Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
        let unique = "ArgumentException " + Guid.NewGuid().ToString()
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation (fun () ->
          let inner = InvalidOperationException()
          ArgumentException(unique, inner) |> raise) () true
        NUnit.Framework.Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
        NUnit.Framework.Assert.That
          (CommandLine.exceptions |> List.map (fun e -> e.Message),
           Is.EquivalentTo [ unique ])
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let there = Path.Combine(here, Guid.NewGuid().ToString())
        let toInfo = Directory.CreateDirectory there
        Visitor.outputDirectories.Clear()
        Visitor.inputDirectories.Clear()
        Visitor.outputDirectories.Add toInfo.FullName
        Visitor.inputDirectories.Add here
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(err.ToString(), Is.Empty)
        let name = "ArgumentExceptionWrites"
        CommandLine.logExceptionsToFile name true
        let target = Path.Combine(toInfo.FullName, name)
        let target' = Path.Combine(here, name)
        NUnit.Framework.Assert.That(File.Exists target, target)
        let lines' =
          target
          |> File.ReadAllLines
          |> Seq.toList

        let head = lines' |> List.head

        let lines = if head.Length > 80
                    then lines'
                    else let t = lines' |> List.tail
                         (head + List.head t) :: (List.tail t)

        NUnit.Framework.Assert.That
          (lines.[0],
           Is.EqualTo
             ("System.ArgumentException: " + unique
              + " ---> System.InvalidOperationException: Operation is not valid due to the current state of the object."),
              sprintf "lines = %A" lines)
        NUnit.Framework.Assert.That
          (lines.[1], Does.StartWith("   --- End of inner exception stack trace ---"))
        NUnit.Framework.Assert.That
          (lines.[2].Replace("+", ".").Trim(),
           Does.StartWith
             ("at Tests.AltCoverTests2.ArgumentExceptionWritesEx@"))
        NUnit.Framework.Assert.That
          (lines.[3].Trim(), Does.StartWith("at AltCover.CommandLine.doPathOperation"))
        NUnit.Framework.Assert.That(lines |> List.skip 4, Is.Not.Empty)
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That
          (err.ToString().Trim().Replace("\r", String.Empty).Replace("\n","|"), Is.EqualTo("Details written to " + target +
                                               "|If this problem was detected in the pre-test instrumentation stage of `dotnet test`, then the file may have been moved to " +
                                               target' + " when the task completes.|"))
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []
        Output.Info <- (fst saved)
        Output.Error <- (snd saved)
        Visitor.outputDirectories.Clear()

    [<Test;TestMethod>]
    let IOExceptionWrites() =
      let saved = (Output.Info, Output.Error)
      let err = System.Text.StringBuilder()
      let info = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
        Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
        let unique = "IOException " + Guid.NewGuid().ToString()
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation (fun () -> IOException(unique) |> raise) () false
        NUnit.Framework.Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
        CommandLine.ReportErrors "Instrumentation" false
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        let logged = err.ToString().Replace("\r", String.Empty).Replace("\n", "|")
        NUnit.Framework.Assert.That
          (logged, Is.EqualTo("|ERROR *** Instrumentation phase failed|||" + unique + "|"))
        NUnit.Framework.Assert.That(CommandLine.exceptions, Is.Empty)
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []
        Output.Info <- (fst saved)
        Output.Error <- (snd saved)

    [<Test;TestMethod>]
    let NotSupportedExceptionWrites() =
      let saved = (Output.Info, Output.Error)
      let err = System.Text.StringBuilder()
      let info = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
        Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
        let unique = "NotSupportedException " + Guid.NewGuid().ToString()
        CommandLine.error <- []
        CommandLine.doPathOperation (fun () -> NotSupportedException(unique) |> raise) ()
          false
        NUnit.Framework.Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(err.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(CommandLine.exceptions, Is.Empty)
      finally
        CommandLine.error <- []
        Output.Info <- (fst saved)
        Output.Error <- (snd saved)

    [<Test;TestMethod>]
    let SecurityExceptionWrites() =
      let saved = (Output.Info, Output.Error)
      let err = System.Text.StringBuilder()
      let info = System.Text.StringBuilder()
      try
        Output.Info <- (fun s -> info.Append(s).Append("|") |> ignore)
        Output.Error <- (fun s -> err.Append(s).Append("|") |> ignore)
        let unique = "SecurityException " + Guid.NewGuid().ToString()
        CommandLine.error <- []
        CommandLine.exceptions <- []
        CommandLine.doPathOperation
          (fun () -> System.Security.SecurityException(unique) |> raise) () false
        NUnit.Framework.Assert.That(CommandLine.error, Is.EquivalentTo [ unique ])
        NUnit.Framework.Assert.That(info.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(err.ToString(), Is.Empty)
        NUnit.Framework.Assert.That(CommandLine.exceptions, Is.Empty)
      finally
        CommandLine.error <- []
        CommandLine.exceptions <- []
        Output.Info <- (fst saved)
        Output.Error <- (snd saved)