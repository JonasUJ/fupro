module Dictionary

type Dict =
    | Leaf
    | Node of value : char * flag : bool * left : Dict * mid : Dict * right : Dict
val empty: unit -> Dict
val insert: string -> Dict -> Dict
val step: char -> Dict -> (bool * Dict) option
val reverse: Dict -> (bool * Dict) option
val lookup: string -> Dict -> bool
