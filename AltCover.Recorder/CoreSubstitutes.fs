namespace System

open System
open System.Collections.Generic

////1>FSC: error FS0193: The module/namespace 'System' from compilation unit 'mscorlib' did not contain the namespace, module or type 'Lazy`1'
////1>FSC: error FS0193: The module/namespace 'System' from compilation unit 'mscorlib' did not contain the namespace, module or type 'IObservable`1'
////1>FSC: error FS0193: The module/namespace 'System' from compilation unit 'mscorlib' did not contain the namespace, module or type 'IObservable`1'
////1>FSC: error FS0193: The module/namespace 'System' from compilation unit 'mscorlib' did not contain the namespace, module or type 'IObservable`1'

[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
type LiteralAttribute() =
   inherit Attribute()

type Lazy<'a>() =
  inherit Object()

type IObserver<'a> =
  abstract member OnNext : 'a -> Void
  abstract member OnError : Exception -> Void
  abstract member OnCompleted : Void -> Void

type IObservable<'a> =
  abstract member Subscribe : IObserver<'a> -> IDisposable


module CoreSubstitutes =

  let ignore _ = ()
  let (|>) x f = f x
  let (<|) f x = f x
  let (>>) f g x = g (f x)
  let fst (a, b) = a
  let snd (a, b) = b
  let max (a:Int32) (b:Int32) = if a > b then a else b
  let not b = if b then false else true


module MySeq =
    open System.Collections

    let cast<'a> (seq:#IEnumerable) =
      let l = List<'a>()
      for x in seq do 
        l.Add (x :?> 'a)
      l
    
    let filter<'a> (f : 'a -> Boolean) (seq : #IEnumerable<'a>) =
      let l = List<'a>()
      for x in seq do 
        if f x then l.Add x
      l
      
    let truncate<'a> n (seq : #IEnumerable<'a>) =
      let l = List<'a>()
      for x in seq do 
        if l.Count < n then l.Add x
      l

    let collect<'a, 'b> (f : 'a -> IEnumerable<'b>) (seq : #IEnumerable<'a>) =
      let l = List<'b>()
      for x in seq do 
        l.AddRange (f x)
      l

    let rev<'a> (seq : #IEnumerable<'a>) =
      let l = List<'a> seq
      l.Reverse()
      l

    let mapi<'a,'b> (f : Int32 -> 'a -> 'b) (seq : #IEnumerable<'a>) =
      let l = List<'b>()
      let mutable index = 0
      for x in seq do 
        l.Add (f index x)
        index <- index + 1
      l



      
