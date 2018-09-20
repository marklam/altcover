namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Globalization
open System.IO
open System.Linq
open System.Xml
open System.Xml.Schema
open System.Xml.XPath
open System.Collections.Generic

module OpenCoverUtilities =

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification = "Premature abstraction")>]
  let MergeNCover inputs =
    let doc = XmlDocument()
    doc.CreateComment(inputs.ToString())
    |> doc.AppendChild
    |> ignore
    //  XmlUtilities.PrependDeclaration xmlDocument
    doc

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification = "Premature abstraction")>]
  let MergeOpenCover(inputs : XmlDocument list) =
    let loadFromString() =
      use reader =
        new StringReader("""<CoverageSession xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"><Summary numSequencePoints="?" visitedSequencePoints="0" numBranchPoints="?" visitedBranchPoints="0" sequenceCoverage="0" branchCoverage="0" maxCyclomaticComplexity="0" minCyclomaticComplexity="0" visitedClasses="0" numClasses="?" visitedMethods="0" numMethods="?" minCrapScore="0" maxCrapScore="0" /><Modules /></CoverageSession>""")
      let doc = XmlDocument()
      doc.Load(reader)
      doc

    let doc = loadFromString()

    let modules =
      inputs
      |> List.collect
           (fun x -> x.SelectNodes("//Module").OfType<XmlElement>() |> Seq.toList)
      |> List.groupBy (fun x -> x.GetAttribute("hash"))

    let results = Dictionary<string, XmlElement>()
    let classes = Dictionary<string, XmlElement>()
    let summaries = List<XmlElement>()
    modules
    |> List.iter (fun (h, l) ->
         let m = doc.CreateElement("Module")
         results.Add(h, m)
         m.SetAttribute("hash", h)
         let s =
           l
           |> List.map (fun x -> x.GetAttribute("skippedDueTo"))
           |> List.distinct
         if s |> List.exists String.IsNullOrWhiteSpace then ()
         else m.SetAttribute("skippedDueTo", String.Join(";", s))
         match l
               |> List.map (fun x ->
                    x.ChildNodes.OfType<XmlElement>()
                    |> Seq.filter (fun n -> n.Name = "Summary")
                    |> Seq.tryHead)
               |> List.choose id
               |> List.tryHead with
         | Some n ->
           summaries.Add n
           doc.ImportNode(n.Clone(), true)
           |> m.AppendChild
           |> ignore
         | None -> ()
         (l |> List.head).ChildNodes.OfType<XmlElement>()
         |> Seq.filter (fun n -> n.Name.StartsWith("Module", StringComparison.Ordinal))
         |> Seq.iter (fun n ->
              doc.ImportNode(n.Clone(), true)
              |> m.AppendChild
              |> ignore)
         // Maybe Files
         let c = doc.CreateElement("Classes")
         classes.Add(h, c)
         c
         |> m.AppendChild
         |> ignore)
    // Maybe TrackedMethods
    let (numSequencePoints, numBranchPoints, maxCyclomaticComplexity,
         minCyclomaticComplexity, numClasses, numMethods) =
      summaries
      |> Seq.fold
           (fun x summary ->
           let (s, b, xcc, ncc, c, m) = x
           (s + (Int32.TryParse(summary.GetAttribute "numSequencePoints") |> snd),
            b + (Int32.TryParse(summary.GetAttribute "numBranchPoints") |> snd),
            Math.Max
              (xcc, Int32.TryParse(summary.GetAttribute "maxCyclomaticComplexity") |> snd),
            Math.Min
              (ncc, Int32.TryParse(summary.GetAttribute "minCyclomaticComplexity") |> snd),
            c + (Int32.TryParse(summary.GetAttribute "numClasses") |> snd),
            m + (Int32.TryParse(summary.GetAttribute "numMethods") |> snd)))
           (0, 0, 1, Int32.MaxValue, 0, 0)

    let summary = doc.SelectSingleNode("//Summary") :?> XmlElement
    summary.SetAttribute
      ("numSequencePoints", numSequencePoints.ToString(CultureInfo.InvariantCulture))
    summary.SetAttribute
      ("numBranchPoints", numBranchPoints.ToString(CultureInfo.InvariantCulture))
    summary.SetAttribute
      ("maxCyclomaticComplexity",
       Math.Max(1, maxCyclomaticComplexity).ToString(CultureInfo.InvariantCulture))
    summary.SetAttribute
      ("minCyclomaticComplexity",
       Math.Max(1, Math.Min(minCyclomaticComplexity, maxCyclomaticComplexity))
           .ToString(CultureInfo.InvariantCulture))
    summary.SetAttribute("numClasses", numClasses.ToString(CultureInfo.InvariantCulture))
    summary.SetAttribute("numMethods", numMethods.ToString(CultureInfo.InvariantCulture))

    // tidy up here
    AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover doc
    XmlUtilities.PrependDeclaration doc
    doc

  [<SuppressMessage("Microsoft.Design", "CA1059", Justification = "Premature abstraction")>]
  let MergeCoverage (documents : IXPathNavigable seq) ncover =
    let inputs =
      documents
      |> Seq.map (fun x ->
           let xmlDocument = new XmlDocument()
           x.CreateNavigator().ReadSubtree() |> xmlDocument.Load
           try
             let format = XmlUtilities.DiscoverFormat xmlDocument
             match (ncover, format) with
             | (true, Base.ReportFormat.NCover) | (false, Base.ReportFormat.OpenCover) ->
               Some xmlDocument
             | _ -> None
           with :? XmlSchemaValidationException -> None)
      |> Seq.choose id
      |> Seq.toList
    match inputs with
    | [] -> XmlDocument()
    | [ x ] ->
      XmlUtilities.PrependDeclaration x
      x
    | _ ->
      if ncover then MergeNCover inputs
      else MergeOpenCover inputs

  let private CompressMethod withinSequencePoint sameSpan (m : XmlElement) =
    let sp = m.GetElementsByTagName("SequencePoint").OfType<XmlElement>() |> Seq.toList
    let bp = m.GetElementsByTagName("BranchPoint").OfType<XmlElement>() |> Seq.toList
    if sp
       |> List.isEmpty
       |> not
       && bp
          |> List.isEmpty
          |> not
    then
      let interleave =
        List.concat [ sp; bp ]
        |> List.sortBy (fun x ->
             x.GetAttribute("offset")
             |> Int32.TryParse
             |> snd)

      interleave
      |> Seq.fold (fun (s : XmlElement, bs : XmlElement list) x ->
           match x.Name with
           | "SequencePoint" ->
             let bx =
               if withinSequencePoint then
                 let next =
                   x.GetAttribute("offset")
                   |> Int32.TryParse
                   |> snd

                 let (kill, keep) =
                   bs
                   |> List.partition (fun b ->
                        b.GetAttribute("offsetend")
                        |> Int32.TryParse
                        |> snd < next)

                 kill |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                 keep
               else bs

             let by =
               if sameSpan then
                 let (kill, keep) =
                   bx
                   |> List.groupBy
                        (fun b ->
                        (b.GetAttribute("offset"), b.GetAttribute("offsetchain"),
                         b.GetAttribute("offsetend")))
                   |> List.fold (fun (ki, ke) (_, bz) ->
                        let totalVisits =
                          bz
                          |> Seq.map (fun b ->
                               b.GetAttribute("vc")
                               |> Int32.TryParse
                               |> snd)
                          |> Seq.sum

                        let h = bz |> Seq.head
                        h.SetAttribute
                          ("vc", totalVisits.ToString(CultureInfo.InvariantCulture))
                        (List.concat [ ki
                                       bz
                                       |> Seq.tail
                                       |> Seq.toList ], h :: ke)) ([], [])
                 kill |> Seq.iter (fun b -> b.ParentNode.RemoveChild(b) |> ignore)
                 keep
               else bx

             // Fix up what remains
             by
             |> List.rev // because the list will have been built up in reverse order
             |> Seq.mapi (fun i b -> (i, b))
             |> Seq.groupBy (fun (_, b) -> b.GetAttribute("offset"))
             |> Seq.iter
                  (fun (_, paths) ->
                  paths // assume likely ranges for these numbers!
                  |> Seq.sortBy (fun (n, p) -> n + 100 * (p.GetAttribute("offsetend")
                                                          |> Int32.TryParse
                                                          |> snd))
                  |> Seq.iteri
                       (fun i (_, p) ->
                       p.SetAttribute
                         ("path", (i + 1).ToString(CultureInfo.InvariantCulture))))
             s.SetAttribute("bec", by.Length.ToString(CultureInfo.InvariantCulture))
             s.SetAttribute("bev", "0")
             (x, [])
           | _ -> (s, x :: bs)) (sp.Head, [])
      |> ignore

  [<SuppressMessage("Microsoft.Design", "CA1059",
                    Justification = "returns a specific concrete type")>]
  let CompressBranching (navigable : IXPathNavigable) withinSequencePoint sameSpan =
    // Validate
    let xmlDocument = new XmlDocument()
    navigable.CreateNavigator().ReadSubtree() |> xmlDocument.Load
    xmlDocument.Schemas <- XmlUtilities.LoadSchema AltCover.Base.ReportFormat.OpenCover
    xmlDocument.Validate(null)
    // Get all the methods
    xmlDocument.SelectNodes("//Method")
    |> Seq.cast<XmlElement>
    |> Seq.iter (CompressMethod withinSequencePoint sameSpan)
    // tidy up here
    AltCover.Runner.PostProcess null AltCover.Base.ReportFormat.OpenCover xmlDocument
    XmlUtilities.PrependDeclaration xmlDocument
    xmlDocument