namespace Tests
// fsharplint:disable  MemberNames NonPublicValuesNames RedundantNewKeyword

open System
open System.IO
open System.Reflection

open AltCover
open AltCover.Augment
open Mono.Options
open Mono.Cecil.Cil

module AltCoverTests3 =
#if NETCOREAPP2_0
    let monoSample1 = "../_Mono/Sample1"
#else
    let recorderSnk = typeof<AltCover.Node>.Assembly.GetManifestResourceNames()
                      |> Seq.find (fun n -> n.EndsWith(".Recorder.snk", StringComparison.Ordinal))
#endif
    // AltCover.fs and CommandLine.fs

    [<Test>]
    let ShouldLaunchWithExpectedOutput() =
      Main.init()
      // Hack for running while instrumented
      let where = Assembly.GetExecutingAssembly().Location
      let path =
        Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
      let path' =
        if Directory.Exists path then path
        else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
      let path' = path
#endif
      let files = Directory.GetFiles(path')

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head

      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      AltCover.toConsole()
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let exe, args =
          if nonWindows then ("mono", "\"" + program + "\"")
          else (program, String.Empty)

        let r =
          CommandLine.I.launch exe args
            (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location))
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        let result = stdout.ToString()

        let quote =
          if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\""
          else String.Empty

        let expected =
          "Command line : '" + quote + exe + quote + " " + args + "\'"
          + Environment.NewLine + "Where is my rocket pack? " + Environment.NewLine
        Assert.That(result, Is.EqualTo(expected))
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let ShouldHaveExpectedOptions() =
      Main.init()
      let options = Main.I.declareOptions()
      Assert.That(options.Count, Is.EqualTo 30)
      Assert.That
        (options
         |> Seq.filter (fun x -> x.Prototype <> "<>")
         |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
      Assert.That(options
                  |> Seq.filter (fun x -> x.Prototype = "<>")
                  |> Seq.length, Is.EqualTo 1)

    [<Test>]
    let ParsingJunkIsAnError() =
      Main.init()
      let options = Main.I.declareOptions()
      let parse = CommandLine.parseCommandLine [| "/@thisIsNotAnOption" |] options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)

    [<Test>]
    let ParsingJunkBeforeSeparatorIsAnError() =
      Main.init()
      let options = Main.I.declareOptions()
      let parse =
        CommandLine.parseCommandLine
          [| "/@thisIsNotAnOption"; "--"; "this should be OK" |] options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)

    [<Test>]
    let ParsingJunkAfterSeparatorIsExpected() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--"; "/@thisIsNotAnOption"; "this should be OK" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(x, Is.EquivalentTo(input |> Seq.skip 1))
        Assert.That(y, Is.SameAs options)

    [<Test>]
    let ParsingHelpGivesHelp() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--?" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) -> Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "HelpText")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      match CommandLine.parseCommandLine [| "/t"; "x" |] options
            |> CommandLine.processHelpOption with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

    [<Test>]
    let ParsingErrorHelpGivesHelp() =
      Main.init()
      let options = Main.I.declareOptions()

      let input =
        [| "--o"
           Path.GetInvalidPathChars() |> String |]

      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      match CommandLine.processHelpOption parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(y, Is.SameAs options)
      // a "not sticky" test
      match CommandLine.parseCommandLine [| "/t"; "x" |] options
            |> CommandLine.processHelpOption with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)

    [<Test>]
    let ParsingAttributesGivesAttributes() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-a"; "1;a"; "--a"; "2"; "/a"; "3"; "-a=4"; "--a=5"; "/a=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 7)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Attribute -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Attribute -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1"; "a"; "2"; "3"; "4"; "5"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingMethodsGivesMethods() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-m"; "1"; "--m"; "2;b;c"; "/m"; "3"; "-m=4"; "--m=5"; "/m=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Method -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Method -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1"; "2"; "b"; "c"; "3"; "4"; "5"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingTypesGivesTypes() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-t"; "1"; "--t"; "2"; "/t"; "3;x;y;z"; "-t=4"; "--t=5"; "/t=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 9)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Type -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Type -> x.Regex.ToString()
                | _ -> "*"),
           Is.EquivalentTo ([| "1"; "2"; "3"; "x"; "y"; "z"; "4"; "5"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingAssembliesGivesAssemblies() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-s"; "?1"; "--s"; "2"; "/s"; "3"; "-s=4;p;q"; "--s=5"; "/s=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Assembly -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Assembly -> x.Regex.ToString()
                | _ -> "*"),
           Is.EquivalentTo ([| "1"; "2"; "3"; "4"; "p"; "q"; "5"; "6" |]) )
        Assert.That
           (CoverageParameters.nameFilters
            |> Seq.map (fun x -> if x.Sense = Include then 1 else 0),
            Is.EquivalentTo ([| 1; 0; 0; 0; 0; 0;  0; 0 |]) )
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingEscapeCasesWork() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-s"; "1\u0001a"; "--s"; "\u0000d"; "/s"; "3"; "-s=4;;p;q"; "--s=5"; "/s=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 7)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Assembly -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Assembly -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1|a"; "\\d"; "3"; "4;p"; "q"; "5"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingModulesGivesModules() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-e"; "1"; "--e"; "2"; "/e"; "3"; "-e=4;p;q"; "--e=5"; "/e=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Module -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Module -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1"; "2"; "3"; "4"; "p"; "q"; "5"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingFilesGivesFiles() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-f"; "1"; "--f"; "2"; "/f"; "3"; "-f=4"; "--f=5;m;n"; "/f=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.File -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.File -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1"; "2"; "3"; "4"; "5"; "m"; "n"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingPathsGivesPaths() =
      Main.init()
      try
        CoverageParameters.nameFilters.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-p"; "1"; "--p"; "2"; "/p"; "3"; "-p=4"; "--p=5;m;n"; "/p=6" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.nameFilters.Count, Is.EqualTo 8)
        Assert.That(CoverageParameters.nameFilters
                    |> Seq.forall (fun x ->
                         match x.Scope with
                         | FilterScope.Path -> true
                         | _ -> false))
        Assert.That
          (CoverageParameters.nameFilters
           |> Seq.map (fun x ->
                match x.Scope with
                | FilterScope.Path -> x.Regex.ToString()
                | _ -> "*"), Is.EquivalentTo ([| "1"; "2"; "3"; "4"; "5"; "m"; "n"; "6" |] ))
        Assert.That
          (CoverageParameters.nameFilters |> Seq.forall (fun x -> x.Sense = Exclude))
      finally
        CoverageParameters.nameFilters.Clear()

    [<Test>]
    let ParsingXmlGivesXml() =
      Main.init()
      try
        CoverageParameters.theReportPath <- None
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let where = Assembly.GetExecutingAssembly().Location
        let path = Path.Combine(Path.GetDirectoryName(where), unique)
        let input = [| "-x"; path |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match CoverageParameters.theReportPath with
        | None -> Assert.Fail()
        | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
      finally
        CoverageParameters.theReportPath <- None

    [<Test>]
    let ParsingMultipleXmlGivesFailure() =
      Main.init()
      try
        CoverageParameters.theReportPath <- None
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-x"
             unique
             "/x"
             unique.Replace("-", "+") |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--xmlReport : specify this only once")
      finally
        CoverageParameters.theReportPath <- None

    [<Test>]
    let ParsingBadXmlGivesFailure() =
      Main.init()
      try
        CoverageParameters.theReportPath <- None
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-x"
             unique.Replace("-", Path.GetInvalidPathChars() |> String) |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theReportPath <- None

    [<Test>]
    let ParsingNoXmlGivesFailure() =
      Main.init()
      try
        CoverageParameters.theReportPath <- None
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-x" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theReportPath <- None

    [<Test>]
    let ParsingEmptyXmlGivesFailure() =
      Main.init()
      try
        CoverageParameters.theReportPath <- None
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-x"; " " |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theReportPath <- None

    [<Test>]
    let ParsingInputGivesInput() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Path.GetFullPath(".")
        let input = [| "-i"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match CoverageParameters.theInputDirectories |> Seq.toList with
        | [ x ] -> Assert.That(x, Is.EqualTo unique)
        | _ -> Assert.Fail()
      finally
        CoverageParameters.theInputDirectories.Clear()

    [<Test>]
    let ParsingMultipleInputIsOKToo() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.inplace := false
        let options = Main.I.declareOptions()

        let input =
          [| "-i"
             Path.GetFullPath(".")
             "/i"
             Path.GetFullPath("..") |]

        let parse = CommandLine.parseCommandLine input options
        let pcom a b = Path.Combine(b,a) |> Path.GetFullPath
        match parse with
        | Left _ -> Assert.Fail()
        | _ -> CoverageParameters.inputDirectories() |> Seq.toList
               |> List.zip ([ "."; ".." ] |> List.map Path.GetFullPath)
               |> List.iter Assert.AreEqual
               CoverageParameters.outputDirectories() |> Seq.toList
               |> List.zip ([ "."; ".." ] |> List.map (pcom "__Instrumented") )
               |> List.iter Assert.AreEqual

               CoverageParameters.inplace := true
               CoverageParameters.theOutputDirectories.Add "maybe"
               CoverageParameters.outputDirectories() |> Seq.toList
               |> List.zip [ Path.GetFullPath "maybe"; ".." |> (pcom "__Saved")]
               |> List.iter Assert.AreEqual

      finally
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.inplace := false

    [<Test>]
    let ParsingDuplicateInputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        let options = Main.I.declareOptions()
        let here = Path.GetFullPath(".")
        let input = [| "-i"; here; "-i"; here |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error,
                      Is.EquivalentTo ([here + " was already specified for --inputDirectory"]))
      finally
        CoverageParameters.theInputDirectories.Clear()

    [<Test>]
    let ParsingBadInputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-i"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theInputDirectories.Clear()

    [<Test>]
    let ParsingNoInputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-i" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theInputDirectories.Clear()

    [<Test>]
    let ParsingOutputGivesOutput() =
      Main.init()
      try
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-o"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match CoverageParameters.outputDirectories() |> Seq.toList with
        | [ x ] -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
        | _ -> Assert.Fail()
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingDuplicateOutputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let input = [| "-o"; unique; "-o"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error,
                      Is.EquivalentTo ([Path.GetFullPath(unique) + " was already specified for --outputDirectory"]))
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingMultipleOutputIsOK() =
      Main.init()
      try
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()
        let u2 = unique.Replace("-", "+")
        let outs = [ unique; u2 ] |> List.map Path.GetFullPath

        let input =
          [| "-o"
             unique
             "/o"
             u2
          |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | _ -> Assert.That (CoverageParameters.theOutputDirectories, Is.EquivalentTo outs)
               Assert.That (CoverageParameters.outputDirectories(), Is.EquivalentTo (outs |> Seq.take 1))
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingBadOutputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString()

        let input =
          [| "-o"
             unique.Replace("-", Path.GetInvalidPathChars() |> String) |]

        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingNoOutputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-o" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingEmptyOutputGivesFailure() =
      Main.init()
      try
        CoverageParameters.theOutputDirectories.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-o"; " " |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theOutputDirectories.Clear()

    [<Test>]
    let ParsingSymbolGivesSymbol() =
      Main.init()
      try
        ProgramDatabase.symbolFolders.Clear()
        let options = Main.I.declareOptions()
        let unique = Path.GetFullPath(".")
        let Symbol = [| "-y"; unique |]
        let parse = CommandLine.parseCommandLine Symbol options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match ProgramDatabase.symbolFolders.Count with
        | 1 -> Assert.That(ProgramDatabase.symbolFolders, Is.EquivalentTo [ unique ])
        | _ -> Assert.Fail()
      finally
        ProgramDatabase.symbolFolders.Clear()

    [<Test>]
    let ParsingMultipleSymbolGivesOK() =
      Main.init()
      try
        ProgramDatabase.symbolFolders.Clear()
        let options = Main.I.declareOptions()

        let Symbol =
          [| "-y"
             Path.GetFullPath(".")
             "/y"
             Path.GetFullPath("..") |]

        let parse = CommandLine.parseCommandLine Symbol options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match ProgramDatabase.symbolFolders.Count with
        | 2 ->
          Assert.That
            (ProgramDatabase.symbolFolders,
             Is.EquivalentTo(Symbol |> Seq.filter (fun x -> x.Length > 2)))
        | _ -> Assert.Fail()
      finally
        ProgramDatabase.symbolFolders.Clear()

    [<Test>]
    let ParsingBadSymbolGivesFailure() =
      Main.init()
      try
        ProgramDatabase.symbolFolders.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let Symbol = [| "-y"; unique |]
        let parse = CommandLine.parseCommandLine Symbol options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        ProgramDatabase.symbolFolders.Clear()

    [<Test>]
    let ParsingNoSymbolGivesFailure() =
      Main.init()
      try
        ProgramDatabase.symbolFolders.Clear()
        let options = Main.I.declareOptions()
        let Symbol = [| "-y" |]
        let parse = CommandLine.parseCommandLine Symbol options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        ProgramDatabase.symbolFolders.Clear()

    [<Test>]
    let ParsingMultipleDependencyIsOk() =
      Main.init()
      try
        Instrument.resolutionTable.Clear()
        let options = Main.I.declareOptions()
        let here = Assembly.GetExecutingAssembly().Location
        let next = Path.Combine(Path.GetDirectoryName here, "AltCover.Recorder.dll")
        let input = [| "-d"; here; "/d"; next |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        let expected =
          Instrument.resolutionTable.Keys
          |> Seq.map (fun a -> Instrument.resolutionTable.[a].Name.Name)
          |> Seq.sort
        Assert.That
          (String.Join(" ", expected), Is.EqualTo("AltCover.Recorder AltCover.Tests"))
      finally
        Instrument.resolutionTable.Clear()

    let ParsingNoDependencyGivesFailure() =
      Main.init()
      try
        Instrument.resolutionTable.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-d" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Instrument.resolutionTable.Clear()

    [<Test>]
    let ParsingBadDependencyGivesFailure() =
      Main.init()
      try
        Instrument.resolutionTable.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-d"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        Instrument.resolutionTable.Clear()

    [<Test>]
    let ParsingNonDependencyGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions()
        let unique = Assembly.GetExecutingAssembly().Location + ".txt"
        let input = [| "-d"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingStrongNameGivesStrongName() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let input = [| "-sn"; Path.Combine(SolutionRoot.location, "Build/Infrastructure.snk") |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right (x, y) -> Assert.That (y, Is.SameAs options)
                          Assert.That (x, Is.Empty)
        match CoverageParameters.defaultStrongNameKey with
        | None -> Assert.Fail()
        | Some x -> let token = x
                                |> KeyStore.tokenOfKey
                                |> List.map (fun x -> x.ToString("x2"))
                    Assert.That (String.Join (String.Empty, token), Is.EqualTo("c02b1a9f5b7cade8"))
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingMultipleStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let path = SolutionRoot.location
        let input = [| "-sn"; Path.Combine(path, "Build/Infrastructure.snk") ;
                       "/sn"; Path.Combine(path, "Build/Recorder.snk") |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
                         Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--strongNameKey : specify this only once")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingBadStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-sn"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingNonStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let unique = Assembly.GetExecutingAssembly().Location
        let input = [| "-sn"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingNoStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let input = [| "-sn" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingMultipleAltStrongNameIsOk() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let path = SolutionRoot.location
        let input = [| "-k"; Path.Combine(path, "Build/Infrastructure.snk");
                       "/k"; Path.Combine(path, "Build/Recorder.snk") |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right (x, y) -> Assert.That (y, Is.SameAs options)
                          Assert.That (x, Is.Empty)
        let expected = CoverageParameters.keys.Keys
                       |> Seq.map (fun x -> String.Join(String.Empty,
                                                        BitConverter.GetBytes(x)
                                                        |> Seq.map (fun x -> x.ToString("x2"))))
                       |> Seq.sort
        Assert.That (String.Join(" ", expected), Is.EqualTo ("4ebffcaabf10ce6a c02b1a9f5b7cade8"))
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingNoAltStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let input = [| "-k" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
      CoverageParameters.defaultStrongNameKey <- None
      CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingBadAltStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-k"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingNonAltsStrongNameGivesFailure() =
      Main.init()
      try
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()
        let options = Main.I.declareOptions ()
        let unique = Assembly.GetExecutingAssembly().Location
        let input = [| "-k"; unique |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left (x, y) -> Assert.That (y, Is.SameAs options)
                         Assert.That (x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defaultStrongNameKey <- None
        CoverageParameters.keys.Clear()

    [<Test>]
    let ParsingLocalGivesLocal() =
      Main.init()
      try
        CoverageParameters.local := false
        let options = Main.I.declareOptions()
        let input = [| "--localSource" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CoverageParameters.local, Is.True)
      finally
        CoverageParameters.local := false

    [<Test>]
    let ParsingMultipleLocalGivesFailure() =
      Main.init()
      try
        CoverageParameters.local := false
        let options = Main.I.declareOptions()
        let input = [| "-l"; "--localSource" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--localSource : specify this only once")
      finally
        CoverageParameters.local := false

    [<Test>]
    let ParsingVisibleGivesVisible() =
      Main.init()
      try
        CoverageParameters.coalesceBranches := false
        let options = Main.I.declareOptions()
        let input = [| "--visibleBranches" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CoverageParameters.coalesceBranches, Is.True)
      finally
        CoverageParameters.coalesceBranches := false

    [<Test>]
    let ParsingMultipleVisibleGivesFailure() =
      Main.init()
      try
        CoverageParameters.coalesceBranches := false
        let options = Main.I.declareOptions()
        let input = [| "-v"; "--visibleBranches" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--visibleBranches : specify this only once")
      finally
        CoverageParameters.coalesceBranches := false

    [<Test>]
    let ParsingStaticGivesStatic() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--showstatic" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)
      Assert.That(CoverageParameters.staticFilter, StaticFilter.AsCovered |> Some |> Is.EqualTo )

    [<Test>]
    let ParsingStaticPlusGivesStatic() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--showstatic:+" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)
      Assert.That(CoverageParameters.staticFilter, StaticFilter.AsCovered |> Some |> Is.EqualTo )

    [<Test>]
    let ParsingStaticPlusPlusGivesStaticPlus() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--showstatic:++" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)
      Assert.That(CoverageParameters.staticFilter, StaticFilter.NoFilter |> Some |> Is.EqualTo )

    [<Test>]
    let ParsingStaticMinusGivesNoStatic() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--showstatic=-" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.Empty)
      Assert.That(CoverageParameters.staticFilter, StaticFilter.Hidden |> Some |> Is.EqualTo )

    [<Test>]
    let ParsingMultipleStaticGivesFailure() =
      Main.init()
      let options = Main.I.declareOptions()
      let input = [| "--showstatic:++"; "--showstatic:-" |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--showstatic : specify this only once")
        Assert.That(CoverageParameters.staticFilter, StaticFilter.NoFilter |> Some |> Is.EqualTo )

    [<Test>]
    let ParsingJunkStaticGivesFailure() =
      Main.init()
      let options = Main.I.declareOptions()
      let tag = Guid.NewGuid().ToString()
      let input = [| "--showstatic:" + tag  |]
      let parse = CommandLine.parseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left(x, y) ->
        Assert.That(y, Is.SameAs options)
        Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(CommandLine.error |> Seq.head, Is.EqualTo ("--showstatic : cannot be '" + tag + "'"))

    [<Test>]
    let ParsingTimeGivesTime() =
      Main.init()
      try
        CoverageParameters.trackingNames.Clear()
        CoverageParameters.theInterval <- None
        let options = Main.I.declareOptions()
        let input = [| "-c"; "5" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.interval(), Is.EqualTo 100)
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingOnlyArabicNumeralsNotThatSortofArabicNumeralsGivesTime() =
      Main.init()
      try
        CoverageParameters.trackingNames.Clear()
        CoverageParameters.theInterval <- None
        let options = Main.I.declareOptions()
        let input = [| "-c"; "٣" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
        Assert.That(CoverageParameters.interval(), Is.EqualTo 0)
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingMultipleTimesGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let path = SolutionRoot.location
        let input = [| "-c"; "3"; "/c"; "5" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CoverageParameters.interval(), Is.EqualTo 10000)
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--callContext : specify this only once")
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingTimeAndNamesGivesOK() =
      Main.init()
      try
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let path = SolutionRoot.location
        let input = [| "-c"; "3"; "/c"; "x"; "--callContext"; "Hello, World!" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.interval(), Is.EqualTo 10000)
        Assert.That(CoverageParameters.trackingNames, Is.EquivalentTo [ "x"; "Hello, World!" ])
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingBadTimeGivesNoOp() =
      Main.init()
      try
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let unique = Guid.NewGuid().ToString().Replace("-", "*")
        let input = [| "-c"; "9" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.interval(), Is.EqualTo 0)
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingNonTimeGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let unique = Assembly.GetExecutingAssembly().Location
        let input = [| "-c"; "99" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingNoTimeGivesFailure() =
      Main.init()
      try
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-c"; " " |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingAfterSingleGivesFailure() =
      Main.init()
      try
        CoverageParameters.single <- true
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()
        let options = Main.I.declareOptions()
        let input = [| "-c"; "3"; "/c"; "x"; "--callContext"; "Hello, World!" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.single <- false
        CoverageParameters.theInterval <- None
        CoverageParameters.trackingNames.Clear()

    [<Test>]
    let ParsingOpenCoverGivesOpenCover() =
      Main.init()
      try
        CoverageParameters.theReportFormat <- None
        let options = Main.I.declareOptions()
        let input = [| "--opencover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        match CoverageParameters.theReportFormat with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
      finally
        CoverageParameters.theReportFormat <- None

    [<Test>]
    let ParsingMultipleOpenCoverGivesFailure() =
      Main.init()
      try
        CoverageParameters.theReportFormat <- None
        let options = Main.I.declareOptions()
        let input = [| "--opencover"; "--opencover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--opencover : specify this only once")
      finally
        CoverageParameters.theReportFormat <- None

    [<Test>]
    let ParsingInPlaceGivesInPlace() =
      Main.init()
      try
        CoverageParameters.inplace := false
        let options = Main.I.declareOptions()
        let input = [| "--inplace" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CoverageParameters.inplace, Is.True)
      finally
        CoverageParameters.inplace := false

    [<Test>]
    let ParsingMultipleInPlaceGivesFailure() =
      Main.init()
      try
        CoverageParameters.inplace := false
        let options = Main.I.declareOptions()
        let input = [| "--inplace"; "--inplace" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--inplace : specify this only once")
      finally
        CoverageParameters.inplace := false

    [<Test>]
    let ParsingSaveGivesSave() =
      Main.init()
      try
        CoverageParameters.collect := false
        let options = Main.I.declareOptions()
        let input = [| "--save" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
          Assert.That(!CoverageParameters.collect, Is.True)
      finally
        CoverageParameters.collect := false

    [<Test>]
    let ParsingMultipleSaveGivesFailure() =
      Main.init()
      try
        CoverageParameters.collect := false
        let options = Main.I.declareOptions()
        let input = [| "--save"; "--save" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--save : specify this only once")
      finally
        CoverageParameters.collect := false

    [<Test>]
    let ParsingSingleGivesSingle() =
      Main.init()
      try
        CoverageParameters.single <- false
        let options = Main.I.declareOptions()
        let input = [| "--single" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.single, Is.True)
      finally
        CoverageParameters.single <- false

    [<Test>]
    let ParsingMultipleSingleGivesFailure() =
      Main.init()
      try
        CoverageParameters.single <- false
        let options = Main.I.declareOptions()
        let input = [| "--single"; "--single" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--single : specify this only once")
      finally
        CoverageParameters.single <- false

    [<Test>]
    let ParsingSingleAfterContextGivesFailure() =
      Main.init()
      try
        CoverageParameters.single <- false
        CoverageParameters.theInterval <- Some 0
        let options = Main.I.declareOptions()
        let input = [| "--single" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.single <- false
        CoverageParameters.theInterval <- None

    [<Test>]
    let ParsingLineCoverGivesLineCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--linecover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)
        match CoverageParameters.theReportFormat with
#if NETCOREAPP2_0
        | None -> ()
#else
        | None -> Assert.Pass()
#endif
        | Some x -> Assert.Fail()
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let OpenCoverIsCompatibleWithLineCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        CoverageParameters.theReportFormat <- None
        let options = Main.I.declareOptions()
        let input = [| "--linecover"; "--opencover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)
        match CoverageParameters.theReportFormat with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
      finally
        CoverageParameters.theReportFormat <- None
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let LineCoverIsCompatibleWithOpenCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        CoverageParameters.theReportFormat <- None
        let options = Main.I.declareOptions()
        let input = [| "--opencover"; "--linecover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.LineOnly)
        match CoverageParameters.theReportFormat with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
      finally
        CoverageParameters.theReportFormat <- None
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let ParsingMultipleLineCoverGivesFailure() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--linecover"; "--linecover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--linecover : specify this only once")
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let LineCoverIsNotCompatibleWithBranchCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--linecover"; "--branchcover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let ParsingBranchCoverGivesBranchCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--branchcover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)
        match CoverageParameters.theReportFormat with
        | None ->
#if NETCOREAPP2_0
          ()
#else
          Assert.Pass()
#endif
        | Some x -> Assert.Fail()
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let OpenCoverIsCompatibleWithBranchCover() =
      Main.init()
      try
        CoverageParameters.theReportFormat <- None
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--branchcover"; "--opencover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)
        match CoverageParameters.theReportFormat with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
      finally
        CoverageParameters.theReportFormat <- None
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let BranchCoverIsCompatibleWithOpenCover() =
      Main.init()
      try
        CoverageParameters.theReportFormat <- None
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--opencover"; "--branchcover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(CoverageParameters.coverstyle, Is.EqualTo CoverStyle.BranchOnly)
        match CoverageParameters.theReportFormat with
        | None -> Assert.Fail()
        | Some x -> Assert.That(x, Is.EqualTo AltCover.Base.ReportFormat.OpenCover)
      finally
        CoverageParameters.coverstyle <- CoverStyle.All
        CoverageParameters.theReportFormat <- None

    [<Test>]
    let ParsingMultipleBranchCoverGivesFailure() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--branchcover"; "--branchcover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--branchcover : specify this only once")
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let BranchCoverIsNotCompatibleWithLineCover() =
      Main.init()
      try
        CoverageParameters.coverstyle <- CoverStyle.All
        let options = Main.I.declareOptions()
        let input = [| "--branchcover"; "--linecover" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.coverstyle <- CoverStyle.All

    [<Test>]
    let ParsingDropGivesDrop() =
      Main.init()
      try
        CommandLine.dropReturnCode := false
        let options = Main.I.declareOptions()
        let input = [| "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(!CommandLine.dropReturnCode, Is.True)
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    let ParsingMultipleDropGivesFailure() =
      Main.init()
      try
        CommandLine.dropReturnCode := false
        let options = Main.I.declareOptions()
        let input = [| "--dropReturnCode"; "--dropReturnCode" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--dropReturnCode : specify this only once")
      finally
        CommandLine.dropReturnCode := false

    [<Test>]
    let ParsingDeferWorks() =
      Main.init()
      try
        CoverageParameters.defer := None
        let options = Main.I.declareOptions()
        let input = [| "--defer" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(Option.isSome !CoverageParameters.defer)
        Assert.That(Option.get !CoverageParameters.defer)
        Assert.That(CoverageParameters.deferOpCode(), Is.EqualTo OpCodes.Ldc_I4_1)
      finally
        CoverageParameters.defer := None

    [<Test>]
    let ParsingDeferPlusWorks() =
      Main.init()
      try
        CoverageParameters.defer := None
        let options = Main.I.declareOptions()
        let input = [| "--defer:+" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(Option.isSome !CoverageParameters.defer)
        Assert.That(Option.get !CoverageParameters.defer)
        Assert.That(CoverageParameters.deferOpCode(), Is.EqualTo OpCodes.Ldc_I4_1)
      finally
        CoverageParameters.defer := None

    [<Test>]
    let ParsingDeferMinusWorks() =
      Main.init()
      try
        CoverageParameters.defer := None
        let options = Main.I.declareOptions()
        let input = [| "--defer:-" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Left _ -> Assert.Fail()
        | Right(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.Empty)
        Assert.That(Option.isSome !CoverageParameters.defer)
        Assert.That(Option.get !CoverageParameters.defer, Is.False)
        Assert.That(CoverageParameters.deferOpCode(), Is.EqualTo OpCodes.Ldc_I4_0)
      finally
        CoverageParameters.defer := None

    [<Test>]
    let ParsingDeferJunkGivesFailure() =
      Main.init()
      try
        CoverageParameters.defer := None
        let options = Main.I.declareOptions()
        let input = [| "--defer:junk" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
      finally
        CoverageParameters.defer := None

    [<Test>]
    let ParsingMultipleDeferGivesFailure() =
      Main.init()
      try
        CoverageParameters.defer := None
        let options = Main.I.declareOptions()
        let input = [| "--defer"; "--defer" |]
        let parse = CommandLine.parseCommandLine input options
        match parse with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(CommandLine.error |> Seq.head, Is.EqualTo "--defer : specify this only once")

      finally
        CoverageParameters.defer := None

    [<Test>]
    let OutputLeftPassesThrough() =
      Main.init()
      let arg = (Guid.NewGuid().ToString(), Main.I.declareOptions())
      let fail = Left arg
      match Main.I.processOutputLocation fail with
      | Right _ -> Assert.Fail()
      | Left x -> Assert.That(x, Is.SameAs arg)

    [<Test>]
    let OutputInPlaceFails() =
      Main.init()
      let options = Main.I.declareOptions()
      let saved = (Console.Out, Console.Error)
      try
        use stdout = new StringWriter()
        use stderr = new StringWriter()
        Console.SetOut stdout
        Console.SetError stderr
        let here = Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location)
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theInputDirectories.Add here
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theOutputDirectories.AddRange CoverageParameters.theInputDirectories
        let arg = ([], options)
        let fail = Right arg
        match Main.I.processOutputLocation fail with
        | Right _ -> Assert.Fail()
        | Left(x, y) ->
          Assert.That(y, Is.SameAs options)
          Assert.That(x, Is.EqualTo "UsageError")
          Assert.That(stderr.ToString(), Is.Empty)
          Assert.That
            (CommandLine.error,
             Is.EquivalentTo [ "From and to directories " +
                               here + " are identical" ])
          Assert.That(stdout.ToString(), Is.Empty)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let OutputToNewPlaceIsOK() =
      Main.init()
      let options = Main.I.declareOptions()
      let saved = (Console.Out, Console.Error)
      AltCover.toConsole()
      CommandLine.error <- []
      try
        use stdout = new StringWriter()
        use stderr = new StringWriter()
        Console.SetOut stdout
        Console.SetError stderr
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theInputDirectories.Add here
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theOutputDirectories.Add (Path.GetDirectoryName here)
        let rest = [ Guid.NewGuid().ToString() ]
        let arg = (rest, options)
        let ok = Right arg
        match Main.I.processOutputLocation ok with
        | Left _ -> Assert.Fail()
        | Right(x, y, z, t) ->
          Assert.That(x, Is.SameAs rest)
          y |> Seq.iter (fun y' -> Assert.That(y'.FullName, Is.EqualTo here))
          z |> Seq.iter (fun z' -> Assert.That(z'.FullName, Is.EqualTo(Path.GetDirectoryName here)))
          t
          |> Seq.zip y
          |> Seq.iter (fun (t', y') -> Assert.That(t'.FullName, Is.EqualTo y'.FullName))
          Assert.That
            (stdout.ToString().Replace("\r", String.Empty),
             Is.EqualTo
               ("Instrumenting files from " + here + "\nWriting files to "
                + (Path.GetDirectoryName here) + "\n"))
          Assert.That(stderr.ToString(), Is.Empty)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let OutputToReallyNewPlaceIsOK() =
      Main.init()
      let options = Main.I.declareOptions()
      AltCover.toConsole()
      let saved = (Console.Out, Console.Error)
      CommandLine.error <- []
      try
        use stdout = new StringWriter()
        use stderr = new StringWriter()
        Console.SetOut stdout
        Console.SetError stderr
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let there = Path.Combine(here, Guid.NewGuid().ToString())
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theInputDirectories.Clear()

        CoverageParameters.theInputDirectories.Add here
        CoverageParameters.theOutputDirectories.Add there
        let rest = [ Guid.NewGuid().ToString() ]
        let arg = (rest, options)
        let ok = Right arg
        Assert.That(Directory.Exists there, Is.False)
        match Main.I.processOutputLocation ok with
        | Left _ -> Assert.Fail()
        | Right(x, y, z, t) ->
          Assert.That(x, Is.SameAs rest)
          y |> Seq.iter (fun y' -> Assert.That(y'.FullName, Is.EqualTo here))
          z |> Seq.iter (fun z' -> Assert.That(z'.FullName, Is.EqualTo there))
          t |> Seq.iter (fun t' -> Assert.That(t'.FullName, Is.EqualTo here))
          Assert.That
            (stdout.ToString().Replace("\r", String.Empty),
             Is.EqualTo
               ("Creating folder " + there + "\nInstrumenting files from " + here
                + "\nWriting files to " + there + "\n"))
          Assert.That(stderr.ToString(), Is.Empty)
          Assert.That(Directory.Exists there)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let InPlaceToExistingPlaceFails() =
      Main.init()
      let options = Main.I.declareOptions()
      let saved = (Console.Out, Console.Error)
      CommandLine.error <- []
      CoverageParameters.inplace := true
      try
        use stdout = new StringWriter()
        use stderr = new StringWriter()
        Console.SetOut stdout
        Console.SetError stderr
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theInputDirectories.Add here
        CoverageParameters.theOutputDirectories.Add (Path.GetDirectoryName here)
        let rest = [ Guid.NewGuid().ToString() ]
        let arg = (rest, options)
        let ok = Right arg
        match Main.I.processOutputLocation ok with
        | Right _ -> Assert.Fail()
        | Left _ ->
          Assert.That(stdout.ToString(), Is.Empty)
          Assert.That(stderr.ToString(), Is.Empty)
          Assert.That
            (CommandLine.error,
             Is.EquivalentTo
               [ "Output directory for saved files " + (CoverageParameters.outputDirectories() |> Seq.head)
                 + " already exists" ])
      finally
        CoverageParameters.inplace := false
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let InPlaceOperationIsAsExpected() =
      Main.init()
      let options = Main.I.declareOptions()
      let saved = (Console.Out, Console.Error)
      CommandLine.error <- []
      CoverageParameters.inplace := true
      try
        use stdout = new StringWriter()
        use stderr = new StringWriter()
        Console.SetOut stdout
        Console.SetError stderr
        let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let there = Path.Combine(here, Guid.NewGuid().ToString())
        CoverageParameters.theOutputDirectories.Clear()
        CoverageParameters.theInputDirectories.Clear()
        CoverageParameters.theInputDirectories.Add here
        CoverageParameters.theOutputDirectories.Add there
        let rest = [ Guid.NewGuid().ToString() ]
        let arg = (rest, options)
        let ok = Right arg
        Assert.That(Directory.Exists there, Is.False)
        match Main.I.processOutputLocation ok with
        | Left _ -> Assert.Fail()
        | Right(x, y, z, t) ->
          Assert.That(x, Is.SameAs rest)
          y |> Seq.iter (fun y' -> Assert.That(y'.FullName, Is.EqualTo here))
          z |> Seq.iter (fun z' -> Assert.That(z'.FullName, Is.EqualTo there))
          t |> Seq.iter (fun t' -> Assert.That(t'.FullName, Is.EqualTo there))
          Assert.That
            (stdout.ToString().Replace("\r", String.Empty),
             Is.EqualTo
               ("Creating folder " + there + "\nSaving files to " + there
                + "\nInstrumenting files in " + here + "\n"))
          Assert.That(stderr.ToString(), Is.Empty)
          Assert.That(Directory.Exists there)
          Assert.That(CoverageParameters.sourceDirectories() |> Seq.head, Is.EqualTo there)
          Assert.That(CoverageParameters.instrumentDirectories() |> Seq.head, Is.EqualTo here)
      finally
        CoverageParameters.inplace := false
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let ImageLoadResilientPassesThrough() =
      Main.init()
      let one = ref false
      let two = ref false
      Main.I.imageLoadResilient (fun () -> one := true) (fun () -> two := true)
      Assert.That(!one)
      Assert.That(!two, Is.False)

    [<Test>]
    let ResilientHandlesIOException() =
      Main.init()
      let one = ref false
      let two = ref false
      Main.I.imageLoadResilient (fun () ->
        IOException("fail") |> raise
        one := true) (fun () -> two := true)
      Assert.That(!one, Is.False)
      Assert.That(!two)

    [<Test>]
    let ResilientHandlesBadImageFormatException() =
      Main.init()
      let one = ref false
      let two = ref false
      Main.I.imageLoadResilient (fun () ->
        BadImageFormatException("fail") |> raise
        one := true) (fun () -> two := true)
      Assert.That(!one, Is.False)
      Assert.That(!two)

    [<Test>]
    let ResilientHandlesArgumentException() =
      Main.init()
      let one = ref false
      let two = ref false
      Main.I.imageLoadResilient (fun () ->
        ArgumentException("fail") |> raise
        one := true) (fun () -> two := true)
      Assert.That(!one, Is.False)
      Assert.That(!two)

    [<Test>]
    let PreparingNewPlaceShouldCopyEverything() =
      Main.init()
      let monoRuntime =
        ("Mono.Runtime"
         |> Type.GetType).IsNotNull
      // because mono symbol-writing is broken, work around trying to
      // examine the instrumented files in a self-test run.
      let here = if monoRuntime
                 then Path.Combine(SolutionRoot.location, "_Binaries/AltCover/Debug+AnyCPU")
                 else Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let there = Path.Combine(here, Guid.NewGuid().ToString())
      let toInfo = [ Directory.CreateDirectory there ]
      let fromInfo = [ DirectoryInfo(here) ]
      let (x, y) = Main.I.prepareTargetFiles fromInfo toInfo fromInfo [there]
      Seq.zip fromInfo toInfo
      |> Seq.iter (fun (f,t) ->
        Assert.That
          (t.EnumerateFiles() |> Seq.map (fun x -> x.Name),
           Is.EquivalentTo(f.EnumerateFiles() |> Seq.map (fun x -> x.Name)),
           "Simple to-from comparison failed")
        Assert.That
          (x
           |> Seq.filter (fun (_,l) -> l |> List.exists (fun i -> i = t.FullName))
           |> Seq.map fst
           |> Seq.filter
                (fun f -> f.EndsWith(".dl_", StringComparison.OrdinalIgnoreCase) |> not)
           |> Seq.filter
                (fun f -> (f |> Path.GetFileName).StartsWith("xunit.", StringComparison.OrdinalIgnoreCase) |> not)
           |> Seq.filter
                (fun f -> (f |> Path.GetFileName).StartsWith("FSharp.", StringComparison.OrdinalIgnoreCase) |> not)
           |> Seq.sort,
           Is.EquivalentTo
             (f.EnumerateFiles()
              |> Seq.map (fun x -> x.FullName)
              |> Seq.filter
                   (fun f ->
                   f.EndsWith(".exe", StringComparison.OrdinalIgnoreCase)
                   || f.EndsWith(".dll", StringComparison.OrdinalIgnoreCase))
              |> Seq.filter (fun f -> f |> Path.GetFileName <> "AltCover.Tests.exe")
              |> Seq.filter
                   (fun f ->
                   File.Exists(Path.ChangeExtension(f, ".pdb")) ||
                   File.Exists(f + ".mdb") ||
                   f |> Path.GetFileNameWithoutExtension = "Sample8")
             |> Seq.sort),
           "First list mismatch with from files")
        Assert.That(y,
                    Is.EquivalentTo(x
                                    |> Seq.map (fst >> Path.GetFileNameWithoutExtension)),
                                    "Second list mismatch"))

    [<Test>]
    let ShouldProcessTrailingArguments() =
      // Hack for running while instrumented
      let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
      let path =
        Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0

      let path' =
        if Directory.Exists path then path
        else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), monoSample1)
#else
      let path' = path
#endif
      let files = Directory.GetFiles(path')

      let program =
        files
        |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
        |> Seq.head

      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      AltCover.toConsole()
      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr
        let u1 = Guid.NewGuid().ToString()
        let u2 = Guid.NewGuid().ToString()
        let baseArgs = [ program; u1; u2 ]
        let nonWindows = System.Environment.GetEnvironmentVariable("OS") <> "Windows_NT"

        let args =
          if nonWindows then "mono" :: baseArgs
          else baseArgs

        let r = CommandLine.processTrailingArguments args (DirectoryInfo(where))
        Assert.That(r, Is.EqualTo 0)
        Assert.That(stderr.ToString(), Is.Empty)
        let result = stdout.ToString()

        let quote =
          if System.Environment.GetEnvironmentVariable("OS") = "Windows_NT" then "\""
          else String.Empty

        let expected =
          "Command line : '" + quote + args.Head + quote + " "
          + String.Join(" ", args.Tail) + "'" + Environment.NewLine
          + "Where is my rocket pack? " + u1 + "*" + u2 + Environment.NewLine
        Assert.That(result, Is.EqualTo expected)
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

    [<Test>]
    let StoresAsExpected() =
      Main.init()
      Api.store <- String.Empty
      Api.logToStore.Info "23"
      Assert.That(Api.store, Is.EqualTo "23")

    [<Test>]
    let ImportModuleIsAsExpected() =
      Main.init()
      AltCover.toConsole()
      let saved = Console.Out
      try
        use stdout = new StringWriter()
        Console.SetOut stdout
        let rc = AltCover.Main.effectiveMain [| "i" |]
        Assert.That(rc, Is.EqualTo 0)
        let result = stdout.ToString().Replace("\r\n", "\n")
        let expected = "Import-Module \""
                       + Path.Combine
                           (Assembly.GetExecutingAssembly().Location
                            |> Path.GetDirectoryName, "AltCover.PowerShell.dll") + """"
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetOut saved

    [<Test>]
    let VersionIsAsExpected() =
      Main.init()
      AltCover.toConsole()
      let saved = Console.Out
      try
        use stdout = new StringWriter()
        Console.SetOut stdout
        let rc = AltCover.Main.effectiveMain [| "v" |]
        Assert.That(rc, Is.EqualTo 0)
        let result = stdout.ToString().Replace("\r\n", "\n")
        let expected = "AltCover version "
                       + AssemblyVersionInformation.AssemblyFileVersion + """
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetOut saved

    [<Test>]
    let UsageIsAsExpected() =
      Main.init()
      let options = Main.I.declareOptions()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let empty = OptionSet()
        CommandLine.usageBase { Intro = "UsageError"; Options = options; Options2 = empty }
        let result = stderr.ToString().Replace("\r\n", "\n")
        let expected = """Error - usage is:
  -i, --inputDirectory=VALUE Optional, multiple: A folder containing assemblies
                               to instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional, multiple: A folder to receive the
                               instrumented assemblies and their companions (
                               default: sub-folder '__Instrumented' of the
                               current directory; or '__Saved' if '--inplace'
                               is set).
                               See also '--inplace'
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
  -d, --dependency=VALUE     Optional, multiple: assembly path to resolve
                               missing reference.
  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -p, --pathFilter=VALUE     Optional, multiple: source file path to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -l, --localSource          Don't instrument code for which the source file is
                               not present.
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
                                   Incompatible with --single
      --opencover            Optional: Generate the report in OpenCover format
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
      --single               Optional: only record the first hit at any
                               location.
                                   Incompatible with --callContext.
      --linecover            Optional: Do not record branch coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --branchcover.
      --branchcover          Optional: Do not record line coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --linecover.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --sourcelink           Optional: Display sourcelink URLs rather than file
                               paths if present.
      --defer[=VALUE]        Optional, defers writing runner-mode coverage data
                               until process exit.
  -v, --visibleBranches      Hide complex internal IL branching implementation
                               details in switch/match constructs, and just
                               show what the source level logic implies.
      --showstatic[=VALUE]   Optional: Instrument and show code that is by
                               default skipped as trivial.  --showstatic:- is
                               equivalent to omitting the parameter; --
                               showstatic or --showstatic:+ sets the unvisited
                               count to a negative value interpreted by the
                               visualizer (but treated as zero by
                               ReportGenerator) ; --showstatic:++ sets the
                               unvisited count to zero.
      --showGenerated        Mark generated code with a visit count of -2 (
                               Automatic) for the Visualizer if unvisited
  -?, --help, -h             Prints out the options.
or
  ImportModule               Prints out the PowerShell script to import the
                               associated PowerShell module
or
  version                    Prints out the AltCover build version
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetError saved

    [<Test>]
    let ErrorResponseIsAsExpected() =
      Main.init()
      let saved = Console.Error
      try
        use stderr = new StringWriter()
        Console.SetError stderr
        let unique = Guid.NewGuid().ToString()
        let main =
          typeof<Node>.Assembly.GetType("AltCover.AltCover")
            .GetMethod("main", BindingFlags.NonPublic ||| BindingFlags.Static)
        let returnCode = main.Invoke(null, [| [| "-i"; unique |] |])
        Assert.That(returnCode, Is.EqualTo 255)
        let result = stderr.ToString().Replace("\r\n", "\n")
        let expected = "\"-i\" \"" + unique + "\"\n" + "--inputDirectory : Directory "
                       + unique + " not found\n" + """Error - usage is:
  -i, --inputDirectory=VALUE Optional, multiple: A folder containing assemblies
                               to instrument (default: current directory)
  -o, --outputDirectory=VALUE
                             Optional, multiple: A folder to receive the
                               instrumented assemblies and their companions (
                               default: sub-folder '__Instrumented' of the
                               current directory; or '__Saved' if '--inplace'
                               is set).
                               See also '--inplace'
  -y, --symbolDirectory=VALUE
                             Optional, multiple: Additional directory to search
                               for matching symbols for the assemblies in the
                               input directory
  -d, --dependency=VALUE     Optional, multiple: assembly path to resolve
                               missing reference.
  -k, --key=VALUE            Optional, multiple: any other strong-name key to
                               use
      --sn, --strongNameKey=VALUE
                             Optional: The default strong naming key to apply
                               to instrumented assemblies (default: None)
  -x, --xmlReport=VALUE      Optional: The output report template file (default:
                                coverage.xml in the current directory)
  -f, --fileFilter=VALUE     Optional, multiple: source file name to exclude
                               from instrumentation
  -p, --pathFilter=VALUE     Optional, multiple: source file path to exclude
                               from instrumentation
  -s, --assemblyFilter=VALUE Optional, multiple: assembly name to exclude from
                               instrumentation
  -e, --assemblyExcludeFilter=VALUE
                             Optional, multiple: assembly which links other
                               instrumented assemblies but for which internal
                               details may be excluded
  -t, --typeFilter=VALUE     Optional, multiple: type name to exclude from
                               instrumentation
  -m, --methodFilter=VALUE   Optional, multiple: method name to exclude from
                               instrumentation
  -a, --attributeFilter=VALUE
                             Optional, multiple: attribute name to exclude from
                               instrumentation
  -l, --localSource          Don't instrument code for which the source file is
                               not present.
  -c, --callContext=VALUE    Optional, multiple: Tracking either times of
                               visits in ticks or designated method calls
                               leading to the visits.
                                   A single digit 0-7 gives the number of
                               decimal places of seconds to report; everything
                               else is at the mercy of the system clock
                               information available through DateTime.UtcNow
                                   A string in brackets "[]" is interpreted as
                               an attribute type name (the trailing "Attribute"
                               is optional), so [Test] or [TestAttribute] will
                               match; if the name contains one or more ".",
                               then it will be matched against the full name of
                               the attribute type.
                                   Other strings are interpreted as method
                               names (fully qualified if the string contains
                               any "." characters).
                                   Incompatible with --single
      --opencover            Optional: Generate the report in OpenCover format
      --inplace              Optional: Instrument the inputDirectory, rather
                               than the outputDirectory (e.g. for dotnet test)
      --save                 Optional: Write raw coverage data to file for
                               later processing
      --single               Optional: only record the first hit at any
                               location.
                                   Incompatible with --callContext.
      --linecover            Optional: Do not record branch coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --branchcover.
      --branchcover          Optional: Do not record line coverage.  Implies,
                               and is compatible with, the --opencover option.
                                   Incompatible with --linecover.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --sourcelink           Optional: Display sourcelink URLs rather than file
                               paths if present.
      --defer[=VALUE]        Optional, defers writing runner-mode coverage data
                               until process exit.
  -v, --visibleBranches      Hide complex internal IL branching implementation
                               details in switch/match constructs, and just
                               show what the source level logic implies.
      --showstatic[=VALUE]   Optional: Instrument and show code that is by
                               default skipped as trivial.  --showstatic:- is
                               equivalent to omitting the parameter; --
                               showstatic or --showstatic:+ sets the unvisited
                               count to a negative value interpreted by the
                               visualizer (but treated as zero by
                               ReportGenerator) ; --showstatic:++ sets the
                               unvisited count to zero.
      --showGenerated        Mark generated code with a visit count of -2 (
                               Automatic) for the Visualizer if unvisited
  -?, --help, -h             Prints out the options.
or
  Runner
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
      --collect              Optional: Process previously saved raw coverage
                               data, rather than launching a process.
  -l, --lcovReport=VALUE     Optional: File for lcov format version of the
                               collected data
  -t, --threshold=VALUE      Optional: minimum acceptable coverage percentage (
                               integer, 0 to 100).  If the coverage result is
                               below threshold, the return code of the process
                               is (threshold - actual) rounded up to the
                               nearest integer.
  -c, --cobertura=VALUE      Optional: File for Cobertura format version of the
                               collected data
  -o, --outputFile=VALUE     Optional: write the recorded coverage to this file
                               rather than overwriting the original report file.
      --dropReturnCode       Optional: Do not report any non-zero return code
                               from a launched process.
      --teamcity[=VALUE]     Optional: Show summary in TeamCity format as well
                               as/instead of the OpenCover summary
  -?, --help, -h             Prints out the options.
"""
        Assert.That
          (result.Replace("\r\n", "\n"), Is.EqualTo(expected.Replace("\r\n", "\n")))
      finally
        Console.SetError saved

    // Tasks.fs
    [<Test>]
    let LoggingCanBeExercised() =
      Main.init()
      Assert.That(FSApi.Logging.ActionAdapter null, Is.Not.Null)
      (FSApi.Logging.ActionAdapter null) "23"
      Assert.That(FSApi.Logging.ActionAdapter(new Action<String>(ignore)), Is.Not.Null)
      let mutable x = String.Empty
      let f = (fun s -> x <- s)
      (FSApi.Logging.ActionAdapter(new Action<String>(f))) "42"
      Assert.That(x, Is.EqualTo "42")
      FSApi.Logging.Create().Info "32"
      FSApi.Logging.Create().Warn "32"
      FSApi.Logging.Create().Error "32"
      FSApi.Logging.Create().Echo "32"

    [<Test>]
    let EmptyInstrumentIsJustTheDefaults() =
      Main.init()
      let subject = Prepare()

      subject.GetType().GetProperties()
      |> Seq.iter (fun p -> let v = p.GetValue(subject)
                            if p.CanWrite then p.SetValue(subject, v))
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      try
        subject.ACLog <- Some <| FSApi.Logging.Create()
        Main.effectiveMain <- (fun a ->
        args <- a
        255)
        let result = subject.Execute()
        Assert.That(result, Is.False)
        Assert.That(args, Is.EquivalentTo [ "--opencover"; "--inplace"; "--save" ])
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved

    [<Test>]
    let NonDefaultInstrumentObsoleteIsOK() =
      Main.init()
      let subject = Prepare()
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      try
        subject.ACLog <- Some <| FSApi.Logging.Create()
        Main.effectiveMain <- (fun a ->
        args <- a
        0)
        subject.OpenCover <- false
        subject.CommandLine <- [| "testing"; "1"; "2"; "3" |]
        subject.SymbolDirectories <- [| "a"; "b" |]
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That
          (args,
           Is.EquivalentTo
             [ "-y"; "a"; "-y"; "b"; "--inplace"; "--save"; "--"; "testing"; "1"; "2"; "3" ])
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved

    [<Test>]
    let NonDefaultInstrumentIsOK() =
      Main.init()
      let subject = Prepare()
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      try
        Main.effectiveMain <- (fun a ->
        args <- a
        0)
        subject.OpenCover <- false
        subject.CommandLine <- [| "testing"; "1"; "2"; "3" |]
        subject.SymbolDirectories <- [| "a"; "b" |]
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That
          (args,
           Is.EquivalentTo
             [ "-y"; "a"; "-y"; "b"; "--inplace"; "--save"; "--"; "testing"; "1"; "2"; "3" ])
        Assert.Throws<InvalidOperationException>(fun () -> subject.Message "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.info "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.warn "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.error "x") |> ignore
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved

    [<Test>]
    let EmptyCollectIsJustTheDefaults() =
      Main.init()
      let subject = Collect()

      subject.GetType().GetProperties()
      |> Seq.iter (fun p -> let v = p.GetValue(subject)
                            if p.CanWrite then p.SetValue(subject, v))
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      try
        subject.ACLog <- Some <| FSApi.Logging.Create()
        Main.effectiveMain <- (fun a ->
        args <- a
        255)
        let result = subject.Execute()
        Assert.That(result, Is.False)
        Assert.That(args, Is.EquivalentTo [ "Runner"; "--collect" ])
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved

    [<Test>]
    let CollectWithExeIsNotCollecting() =
      Main.init()
      let subject = Collect()
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      try
        Main.effectiveMain <- (fun a ->
        args <- a
        0)
        subject.Executable <- "dotnet"
        subject.CommandLine <- [| "test" |]
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That(args, Is.EquivalentTo [ "Runner"; "-x"; "dotnet"; "--"; "test" ])
        Assert.Throws<InvalidOperationException>(fun () -> subject.Message "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.info "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.warn "x") |> ignore
        Assert.Throws<InvalidOperationException>(fun () -> Output.error "x") |> ignore
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved

    [<Test>]
    let EmptyPowerShellIsJustTheDefaults() =
      Main.init()
      let subject = PowerShell()
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      let warned = Output.warn
      Assert.Throws<InvalidOperationException>(fun () -> subject.IO.Warn "x") |> ignore
      Assert.Throws<InvalidOperationException>(fun () -> subject.IO.Error "x") |> ignore
      subject.IO <- FSApi.Logging.Create()
      try
        Main.effectiveMain <- (fun a ->
        args <- a
        0)
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That(args, Is.EquivalentTo [ "ImportModule" ])
        Output.warn "x"
        Output.error "x"
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved
        Output.warn <- warned

    [<Test>]
    let EmptyVersionIsJustTheDefaults() =
      Main.init()
      let subject = GetVersion()
      let save = Main.effectiveMain
      let mutable args = [| "some junk " |]
      let saved = (Output.info, Output.error)
      let warned = Output.warn
      Assert.Throws<InvalidOperationException>(fun () -> subject.IO.Warn "x") |> ignore
      Assert.Throws<InvalidOperationException>(fun () -> subject.IO.Error "x") |> ignore
      subject.IO <- FSApi.Logging.Create()
      try
        Main.effectiveMain <- (fun a ->
        args <- a
        0)
        let result = subject.Execute()
        Assert.That(result, Is.True)
        Assert.That(args, Is.EquivalentTo [ "version" ])
        Output.warn "x"
        Output.error "x"
      finally
        Main.effectiveMain <- save
        Output.info <- fst saved
        Output.error <- snd saved
        Output.warn <- warned

    [<Test>]
    let EchoWorks() =
      Main.init()
      let saved = (Console.Out, Console.Error)
      let e0 = Console.Out.Encoding
      let e1 = Console.Error.Encoding
      let before = Console.ForegroundColor

      try
        use stdout =
          { new StringWriter() with
              member self.Encoding = e0 }

        use stderr =
          { new StringWriter() with
              member self.Encoding = e1 }

        Console.SetOut stdout
        Console.SetError stderr

        let subject = Echo()
        let unique = Guid.NewGuid().ToString()
        subject.Text <- unique
        subject.Colour <- "cyan"
        Assert.That (subject.Execute(), Is.True)
        Assert.That (Console.ForegroundColor, Is.EqualTo before)
        Assert.That (stderr.ToString(), Is.Empty)
        Assert.That (stdout.ToString(), Is.EqualTo (unique + Environment.NewLine))
      finally
        Console.SetOut(fst saved)
        Console.SetError(snd saved)

#if NETCOREAPP2_0
    [<Test>]
    let RunSettingsFailsIfCollectorNotFound() =
      Main.init()
      let subject = RunSettings()
      subject.DataCollector <- Guid.NewGuid().ToString();
      Assert.That (subject.Execute(), Is.False)
      Assert.That (subject.Extended, Is.Empty)

    let template = """<?xml version="1.0" encoding="utf-8"?>
<RunSettings>
{1}  <InProcDataCollectionRunSettings>
    <InProcDataCollectors>
      <InProcDataCollector friendlyName="AltCover" uri="InProcDataCollector://AltCover/Recorder/1.0.0.0" assemblyQualifiedName="AltCover.DataCollector, {2}" codebase="{0}">
        <Configuration>
          <Offload>true</Offload>
        </Configuration>
      </InProcDataCollector>
    </InProcDataCollectors>
  </InProcDataCollectionRunSettings>
</RunSettings>"""

    [<Test>]
    let RunSettingsWorksIfOK() =
      Main.init()
      let subject = RunSettings()
      subject.DataCollector <- Assembly.GetExecutingAssembly().Location
      let assembly = AssemblyName.GetAssemblyName <| Assembly.GetExecutingAssembly().Location
      Assert.That (subject.Execute(), Is.True)
      Assert.That (subject.Extended.EndsWith(".altcover.runsettings"))
      let result = subject.Extended
                   |> File.ReadAllText
      Assert.That (result.Replace("\r", String.Empty).Replace("Collector://AltCover/Recorder/" + assembly.Version.ToString(),
                                                              "Collector://AltCover/Recorder/1.0.0.0"),
                    Is.EqualTo ((String.Format(template,
                                               Assembly.GetExecutingAssembly().Location,
                                               String.Empty,
                                               Assembly.GetExecutingAssembly().FullName)).Replace("\r", String.Empty)))

    [<Test>]
    let RunSettingsExtendsOK() =
      Main.init()
      let subject = RunSettings()
      subject.DataCollector <- Assembly.GetExecutingAssembly().Location
      let assembly = AssemblyName.GetAssemblyName <| Assembly.GetExecutingAssembly().Location
      let settings = Path.GetTempFileName()
      File.WriteAllText(settings, "<RunSettings><stuff /></RunSettings>")
      subject.TestSetting <- settings
      Assert.That (subject.Execute(), Is.True)
      Assert.That (subject.Extended.EndsWith(".altcover.runsettings"))
      let result = subject.Extended
                   |> File.ReadAllText
      Assert.That (result.Replace("\r", String.Empty).Replace("Collector://AltCover/Recorder/" + assembly.Version.ToString(),
                                                              "Collector://AltCover/Recorder/1.0.0.0"),
                    Is.EqualTo ((String.Format(template,
                                               Assembly.GetExecutingAssembly().Location,
                                               "  <stuff />\r\n",
                                               Assembly.GetExecutingAssembly().FullName)).Replace("\r", String.Empty)))

    [<Test>]
    let RunSettingsRecoversOK() =
      Main.init()
      let subject = RunSettings()
      subject.DataCollector <- Assembly.GetExecutingAssembly().Location
      let assembly = AssemblyName.GetAssemblyName <| Assembly.GetExecutingAssembly().Location
      let settings = Path.GetTempFileName()
      File.WriteAllText(settings, "<Not XML")
      subject.TestSetting <- settings
      Assert.That (subject.Execute(), Is.True)
      Assert.That (subject.Extended.EndsWith(".altcover.runsettings"))
      let result = subject.Extended
                   |> File.ReadAllText
      Assert.That (result.Replace("\r", String.Empty).Replace("Collector://AltCover/Recorder/" + assembly.Version.ToString(),
                                                              "Collector://AltCover/Recorder/1.0.0.0"),
                    Is.EqualTo ((String.Format(template,
                                               Assembly.GetExecutingAssembly().Location,
                                               String.Empty,
                                               Assembly.GetExecutingAssembly().FullName)).Replace("\r", String.Empty)))
#endif
  // Recorder.fs => Recorder.Tests