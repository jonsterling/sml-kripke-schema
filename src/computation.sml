signature COMPUTATION =
sig
  val eval : Term.t -> Term.t
  exception Stuck
end

functor Computation (CreatingSubject : CREATING_SUBJECT) : COMPUTATION =
struct
  open Term Operator
  infix $$ $ \\ \ //

  exception Stuck

  fun assertCanonical tm =
    case out tm of
         AX $ _ => tm
       | LAM $ _ => tm
       | INL $ _ => tm
       | INR $ _ => tm
       | PAIR $ _ => tm
       | TRUE $ _ => tm
       | FALSE $ _ => tm
       | OR $ _ => tm
       | AND $ _ => tm
       | IMPLIES $ _ => tm
       | _ => raise Stuck

  fun eval tm =
    case out tm of
         AP $ #[M,N] =>
           (case out (eval M) of
                 LAM $ #[xE] => eval (xE // N)
               | _ => raise Stuck)
       | DECIDE $ #[M, xL, yR] =>
           (case out (eval M) of
                 INL $ #[M'] => eval (xL // M')
               | INR $ #[M'] => eval (yR // M')
               | _ => raise Stuck)
       | DESTRUCT $ #[M, xyE] =>
           (case out (eval M) of
                 PAIR $ #[L,R] => (xyE // L) // R
               | _ => raise Stuck)
       | ORACLE $ #[A] =>
           let
             open CreatingSubject
             val A' = eval A
             val alpha = kripkeSchema {prop = A'}
             val now = stageIndex (stage ())
           in
             if ChoiceSequence.nth alpha now then
               INL $$ #[AX $$ #[]]
             else
               INR $$ #[AX $$ #[]]
           end
       | _ => assertCanonical tm
end
