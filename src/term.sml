structure Variable = Variable ()

structure Term = Abt
  (structure Operator = Operator
   and Variable = Variable)

structure Term = AbtUtil (Term)
