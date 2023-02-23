module MultiSet

open System

let flip f a b = f b a

type MultiSet<'a when 'a: comparison> = S of Map<'a, uint32>

let change key f (S map) =
    match Map.tryFind key map |> f with
    | None -> Map.remove key map
    | Some v -> Map.add key v map
    |> S

let empty = S Map.empty

let size (S s) =
    Map.fold (fun state _ v -> state + v) 0u s

let isEmpty s = (size s) = 0u
let numItems a (S s) = Map.tryFind a s |> Option.defaultValue 0u
let contains a s = (numItems a s) > 0u

let add a n s =
    change a (Option.defaultValue 0u >> (+) n >> Some) s

let addSingle a s = add a 1u s

let remove a n s =
    change a (Option.defaultValue 0u >> (fun x -> if x > n then x - n else 0u) >> Some) s

let removeSingle a s = remove a 1u s
let fold f acc (S s) = Map.fold f acc s
let foldBack f (S s) acc = Map.foldBack f s acc
let ofList lst = List.fold (flip addSingle) empty lst

let toList (S s) =
    List.collect (fun (k, v) -> List.replicate (int v) k) (Map.toList s)

let map f s =
    fold (fun state k v -> add (f k) v state) empty s

let union (S s1) s2 =
    Map.map (fun k v -> Math.Max(v, numItems k s2)) s1 |> S

let sum s1 s2 = fold (fun s k v -> add k v s) s1 s2
let subtract s1 s2 = fold (fun s k v -> remove k v s) s1 s2

let intersection (S s1) s2 =
    Map.map (fun k v -> Math.Min(v, numItems k s2)) s1 |> S
