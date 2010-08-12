type Name = String

data Primitive = Int 
               | String

data Variable = Variable Name

data Expr = Expr Primitive
          | Expr Variable
          | Combination [Expr]
          | Abstraction [Variable] [Expr]

eval :: expr -> expr
