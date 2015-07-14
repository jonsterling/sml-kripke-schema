structure Operator =
struct
  datatype t =
      TRUE
    | FALSE
    | OR
    | AND
    | IMPLIES
    | AX
    | LAM | AP
    | INL | INR | DECIDE
    | PAIR | DESTRUCT
    | ORACLE

  val eq : t * t -> bool = op=

  fun arity TRUE = #[]
    | arity FALSE = #[]
    | arity OR = #[0,0]
    | arity AND = #[0,0]
    | arity IMPLIES = #[0,0]
    | arity AX = #[]
    | arity LAM = #[1]
    | arity AP = #[0,0]
    | arity INL = #[0]
    | arity INR = #[0]
    | arity DECIDE = #[0,1,1]
    | arity PAIR = #[0,0]
    | arity DESTRUCT = #[0,2]
    | arity ORACLE = #[0]

  fun toString TRUE = "true"
    | toString FALSE = "false"
    | toString OR = "or"
    | toString AND = "and"
    | toString IMPLIES = "implies"
    | toString AX = "ax"
    | toString LAM = "λ"
    | toString AP = "ap"
    | toString INL = "inl"
    | toString INR = "inr"
    | toString DECIDE = "decide"
    | toString PAIR = "pair"
    | toString DESTRUCT = "destruct"
    | toString ORACLE = "oracle"
end
