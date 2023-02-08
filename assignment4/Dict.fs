namespace assignment4

// Exercise 2c

module Dict =
    let reflect mirror i lst =
        Seq.toList lst |> List.splitAt i |> (fun (a, b) -> List.rev a @ (mirror :: b))

    let reflections seq =
        [ for i, _ in Seq.indexed seq do
              yield reflect (char 0) (i + 1) seq ]

    type Dict = D of Map<char, bool * Dict>
    let empty () = D Map.empty
    let step key (D dict) = dict.TryFind key

    let folder f key =
        function
        | None -> Some(false, Map.empty.Change(key, f) |> D)
        | Some (mark, D d) -> Some(mark, d.Change(key, f) |> D)

    let insertOnce (dict: Dict) word =
        word
        |> Seq.toList
        |> List.rev
        |> List.fold folder (Some(true, empty ()) |> Option.orElse >> Option.map (fun (_, d) -> (true, d)))
        <| Some(false, dict)
        |> Option.get
        |> snd

    let insert (word: string) dict =
        word |> reflections |> Seq.fold insertOnce dict

    let reverse dict = step (char 0) dict

    let lookup (word: string) dict =
        List.insertAt 1 (char 0) (Seq.toList word)
        |> Seq.fold (fun d key -> Option.bind (snd >> step key) d) (Some(false, dict))
        |> Option.map fst
        |> Option.contains true
