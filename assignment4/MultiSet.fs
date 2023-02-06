namespace assignment4

open System

module MultiSet =
    let flip f a b = f b a

    type MultiSet<'a when 'a: comparison> = S of Map<'a, uint32>
    let empty = S Map.empty
    let isEmpty (S s) = s.IsEmpty
    let size (S s) = Seq.sum s.Values
    let numItems a (S s) = s.TryFind a |> Option.defaultValue 0u
    let contains a (s: MultiSet<'a>) = (numItems a s) > 0u

    let add a n (S s) =
        s.Change(a, Option.defaultValue 0u >> (+) n >> Some) |> S

    let addSingle a (s: MultiSet<'a>) = add a 1u s

    let remove a n (S s) =
        s.Change(a, Option.defaultValue 0u >> (fun x -> if x > n then x - n else 0u) >> Some)
        |> S

    let removeSingle a (s: MultiSet<'a>) = remove a 1u s
    let fold f acc (S s) = Map.fold f acc s
    let foldBack f (S s) acc = Map.foldBack f s acc
    let ofList lst = List.fold (flip addSingle) empty lst

    let toList (S s) =
        List.collect (fun (k, v) -> List.replicate (int v) k) (Map.toList s)

    let map f (s: MultiSet<'a>) = s |> toList |> List.map f |> ofList

    let union (S s1) (s2: MultiSet<'a>) =
        Map.map (fun k v -> Math.Max(v, numItems k s2)) s1 |> S

    let sum (s1: MultiSet<'a>) (s2: MultiSet<'a>) = fold (fun s k v -> add k v s) s1 s2
    let subtract (s1: MultiSet<'a>) (s2: MultiSet<'a>) = fold (fun s k v -> remove k v s) s1 s2

    let intersection (S s1) (s2: MultiSet<'a>) =
        Map.map (fun k v -> Math.Min(numItems k (S s1), numItems k s2)) s1 |> S
