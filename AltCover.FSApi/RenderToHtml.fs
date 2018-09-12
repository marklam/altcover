namespace AltCover

#if TODO
open System.Diagnostics.CodeAnalysis
open System.Xml

module RenderToHtml =
  [<SuppressMessage("Microsoft.Design", "CA1059",
    Justification="Premature abstraction")>]
  let Action x =
    let doc = XmlDocument()
    doc.CreateComment(x.ToString()) |> doc.AppendChild |> ignore
    doc
#endif