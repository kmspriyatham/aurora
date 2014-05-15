import Data.List

data Operator = Add
              | Subtract
              | Multiply
              | Power
                deriving (Eq)

instance Show Operator where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Power = "^" 

data Expression = Constant Integer
                | Variable [Char]
                | Expression Operator [Expression]
                  deriving (Eq)

instance Show Expression where
    show (Variable x) = x
    show (Constant x) = show x
    show (Expression f [x]) = show x
    show (Expression f (x:xs)) = (show x) ++ (show f) ++ (show (Expression f xs))

instance Num Expression where
    x + y = simplify (Expression Add [x, y])
    x * y = simplify (Expression Multiply [x, y])

    fromInteger x = Constant x

simplify (Expression Add (x:xs)) = let rest = filter (Constant 0 /= ) (x:xs) in
                                   if length rest == 1
                                       then simplify (head rest)
                                       else if length rest == 0
                                           then Constant 0
                                           else simplify (Expression Add rest)
simplify (Expression Multiply (x:xs)) = if find (Constant 0 == ) (x:xs) == Nothing
                                          then Expression Multiply (x:xs)
                                          else Constant 0
simplify (Expression Power [0, c]) = Constant 0    --TODO raise exception if c <= 0
simplify (Expression Multiply (x:xs)) = simplify (Expression Multiply (filter (Constant 1 /= ) (x:xs)))
simplify (Expression Power [x, 1]) = simplify x
simplify (Expression Power [x, 0]) = Constant 1
simplify (Expression Power [1, x]) = Constant 1
simplify (Constant x) = Constant x
simplify (Variable x) = Variable x
simplify x = x --Might wanna remove this later