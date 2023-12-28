module AdventOfCode.Day25

open System


[<RequireQualifiedAccess>]
module List =

  let replace (oldElem : 'a) (newElem : 'a) (xs : 'a list) =
    List.map (fun x -> if x = oldElem then newElem else x) xs


type Graph = Map<string, string list>

[<RequireQualifiedAccess>]
module Graph =

  let addAdjacency (a : string) (b : string) (g : Graph) =
    let addDirected a b g =
      match Map.tryFind a g with
      | Some adjs -> Map.add a (b :: adjs) g
      | None -> Map.add a [ b ] g
    (addDirected a b << addDirected b a) g

  let parse (lines : string seq) =
    Seq.fold
      (fun (g : Graph) (line : string) ->
        let parts = line.Split ": "
        let a = parts[0]
        let bs = parts[1].Split ' '
        Seq.fold (fun g b -> addAdjacency a b g) g bs)
      Map.empty lines
  
  type Cut = { Size : int; C1 : int; C2 : int }

  // Karger's algorithm
  let findCut (graph : Graph) (r : Random) =
    let componentSize = Map.map (fun _ _ -> 1) graph
    let rebind oldNode newNode g =
      List.fold
        (fun g n -> Map.add n (List.replace oldNode newNode g[n]) g)
        g g[oldNode]
    let rec merge id (g, csize) =
      if Map.count g = 2 then (g, csize)
      else
        let u = Seq.item (r.Next (Map.count g)) g.Keys
        let v = List.item (r.Next (List.length g[u])) g[u]
        let merged = $"merge-{id}"
        let adj = (List.filter ((<>) v) g[u]) @ (List.filter ((<>) u) g[v])
        let g' =
          g
          |> Map.add merged adj
          |> rebind u merged
          |> rebind v merged
          |> Map.remove u
          |> Map.remove v
        let csize' =
          Map.add merged (Map.find u csize + Map.find v csize) csize
        merge (id + 1) (g', csize')
    let graph', componentSize' = merge 0 (graph, componentSize)
    match Seq.toList (Map.keys graph') with
    | [ a; b ] ->
      let size = List.length graph'[a]
      let c1 = componentSize'[a]
      let c2 = componentSize'[b]
      { Size = size; C1 = c1; C2 = c2 }    
    | _ -> failwith "failed to find cut"


module Puzzle1 =

  open System.IO

  let solve (input : string) =
    let graph = Graph.parse (File.ReadAllLines input)
    let r = Random 25
    let rec loop () =
      let cut = Graph.findCut graph r
      if cut.Size <> 3 then loop ()
      else cut.C1 * cut.C2
    loop ()
