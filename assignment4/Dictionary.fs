module Dictionary

// Exercise 2c

let flip f a b = f b a

let reflect mirror i seq =
    Seq.take i seq |> Seq.rev |> Seq.append
    <| (Seq.ofList [ mirror ] |> Seq.append <| Seq.skip i seq)

let reflections seq =
    Seq.fold
        (fun state (i, _) -> Seq.ofList [ (reflect (char 0) (i + 1) seq) ] |> Seq.append state)
        Seq.empty
        (Seq.indexed seq)

type Dict = D of Map<char, bool * Dict>
let empty () = D Map.empty
let step key (D dict) = dict.TryFind key

let insertSingle key flag map =
    match Map.tryFind key map with
    | None -> Map.add key (flag, empty ()) map
    | Some (curFlag, next) -> Map.add key (curFlag || flag, next) map

let rec insertOnce map (word: char list) =
    match word with
    | [] -> map
    | [ last ] -> insertSingle last true map
    | head :: tail ->
        match Map.tryFind head map with
        | None -> (false, insertOnce Map.empty tail |> D) |> Map.add head <| map
        | Some (flag, D subtree) -> (flag, insertOnce subtree tail |> D) |> Map.add head <| map

let insert (word: string) (D map) =
    Seq.fold insertOnce map (reflections word |> Seq.map Seq.toList) |> D

let reverse dict = step (char 0) dict

let lookup (word: string) dict =
    reflect (char 0) 1 (Seq.toList word)
    |> Seq.fold (fun d key -> Option.bind (snd >> step key) d) (Some(false, dict))
    |> Option.map fst
    |> Option.contains true
