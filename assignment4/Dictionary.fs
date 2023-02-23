module Dictionary

// Exercise 2c

// Ternary search tree with path copying
type Dict =
    | Leaf
    | Node of value: char * flag: bool * left: Dict * mid: Dict * right: Dict

let flip f a b = f b a

// Mirrors an array around an element
// Example: reflect '#' 3 "hello" = "leh#lo"
let reflect mirror i arr =
    (Array.take i arr |> Array.rev) |> Array.append <| [| mirror |] |> Array.append
    <| Array.skip i arr

// Returns all reflections of an array
// Example: reflections '#' "hello" = "h#ello"; "eh#llo"; "leh#lo"; "lleh#o"; "olleh#"
let reflections mirror arr =
    Array.fold (fun state (i, _) -> reflect mirror (i + 1) arr :: state) [] (Array.indexed arr)

let empty () = Leaf

let rec step key =
    function
    | Leaf -> None
    | Node (value = v; left = l) when key < v -> step key l
    | Node (value = v; right = r) when key > v -> step key r
    | Node (flag = f; mid = m) -> Some(f, m)

let reverse dict = step (char 0) dict

let lookup (word: string) dict =
    Array.ofSeq word
    |> reflect (char 0) 1
    |> Seq.fold (fun d key -> Option.bind (snd >> step key) d) (Some(false, dict))
    |> Option.map fst
    |> Option.contains true

let rec insertOnce (word: char[]) idx map =
    if word.Length = idx then
        map
    else
        let key = word.[idx]
        let isLast = word.Length = idx + 1
        match map with
        | Leaf -> Node(key, isLast, Leaf, insertOnce word (idx + 1) Leaf, Leaf)
        | Node (value, flag, left, mid, right) when key < value -> Node(value, flag, insertOnce word idx left, mid, right)
        | Node (value, flag, left, mid, right) when key > value -> Node(value, flag, left, mid, insertOnce word idx right)
        | Node (value, flag, left, mid, right) -> Node(value, flag || isLast, left, insertOnce word (idx + 1) mid, right)

let insert (word: string) map =
    Array.ofSeq word |> reflections (char 0) |> List.fold (fun state value -> insertOnce value 0 state) map
