structure Lcf : LCF =
struct
  type goal = {prop : Term.t}
  type evidence = Term.t
  type validation = evidence list -> evidence
  type tactic = goal -> goal list * validation
end

signature CREATING_SUBJECT =
sig
  structure ChoiceSequence : CHOICE_SEQUENCE

  type stage
  val stage : unit -> stage
  val stageIndex : stage -> Nat.t

  val kripkeSchema : Lcf.goal -> bool ChoiceSequence.t
  val experience : Lcf.goal * Lcf.tactic -> stage
end

functor CreatingSubject (ChoiceSequence : CHOICE_SEQUENCE) :> CREATING_SUBJECT =
struct
  structure ChoiceSequence = ChoiceSequence

  structure JudgementKey =
  struct
    type t = Lcf.goal
    fun eq (x : Lcf.goal, y : Lcf.goal) = Term.eq (#prop x, #prop y)
    fun compare (x : Lcf.goal, y : Lcf.goal) =
      String.compare
        (Term.toString (#prop x),
         Term.toString (#prop y))
  end

  structure Set = SplaySet(structure Elem = JudgementKey)
  type stage = Nat.t
  fun stageIndex w = w

  structure StageHashable : HASHABLE =
  struct
    type t = stage
    fun eq (m, n) =
      Nat.asInt m = Nat.asInt n
    val hash = IntHashable.hash o Nat.asInt
  end

  structure Memory = HashTable (structure Key = StageHashable)
  type memory = Set.set Memory.table
  val memory : memory = Memory.table 1000

  val currentStage : stage ref =
    ref (Nat.fromInt 0)

  fun stage () = ! currentStage

  fun kripkeSchema judgement =
    ChoiceSequence.unfold (fn n =>
      Set.member (Memory.lookup memory n) judgement)

  fun experience (judgement, tactic : Lcf.tactic) =
    case #1 (tactic judgement) of
         _::_ => raise Fail "Judgement not evident"
       | [] =>
         let
           val n = ! currentStage
           val old = Memory.lookup memory n
           val new = Set.insert old judgement
           val n' = Nat.into (Nat.SUCC n)
         in
           currentStage := n';
           Memory.insert memory n' new;
           n'
         end
end
