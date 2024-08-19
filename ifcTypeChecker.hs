type Identifier = String

type EContext = [(Identifier, Value)]
type TContext = [(Identifier, SecType)]
type ErrorMesg = String

data Value = BoolVal Bool
           | NumVal Int
           | StringVal String
           | LamVal Identifier SecType Expr

data Expr = Var String
          | NExp Int
          | BExp Bool
          | Plus Expr Expr
          | Conj Expr Expr
          | Ifelse Expr Expr Expr
          | Lam Identifier SecType Expr
          | App Expr Expr

data Type = TNat
          | TBool
          | Arr SecType SecType
   deriving Eq

data Label = Public
           | Secret
  deriving Eq

-- Define a SecType that includes both the type and a security label
data SecType = Lab Type Label
  deriving (Eq)

join :: Label -> Label -> Label
join Public Public = Public
join _ Secret = Secret
join Secret _ = Secret

leq :: Label -> Label -> Bool
leq Public Public = True
leq Public Secret = True
leq Secret Secret = True
leq Secret Public = False

labelOf :: SecType -> Label
labelOf (Lab _ label) = label



typeCheck :: TContext -> Expr -> Either SecType ErrorMesg
typeCheck g (Var y) = case lookup y g of
                        Just t -> Left t
                        Nothing -> Right "Error: Variable not in context"

typeCheck g (NExp n) = Left (Lab TNat Public)
typeCheck g (BExp b) = Left (Lab TBool Public)

typeCheck g (Plus e1 e2) = 
    case (typeCheck g e1, typeCheck g e2) of
        (Left (Lab TNat label1), Left (Lab TNat label2)) ->
            Left $ Lab TNat (join label1 label2)
        (Right err, _) -> Right err
        (_, Right err) -> Right err
        _ -> Right "Error: Type mismatch in plus expression"


typeCheck g (Ifelse e e1 e2) = do
    case (typeCheck g e, typeCheck g e1, typeCheck g e2) of
        (Left (Lab TBool label), Left (Lab t1' label1), Left (Lab t2' label2))
            | t1' == t2' && leq label label1 && leq label label2 ->
                Left $ Lab t1' (join label1 label2)
        _ -> Right "Error: Type mismatch in if-else expression"

typeCheck g (Lam x t1 e) = 
    let t2' = typeCheck ((x, t1) : g) e in
    case t2' of
        Left t2 ->
            if leq (labelOf t1) (labelOf t2)
            then Left (Lab (Arr t1 t2) (join (labelOf t1) (labelOf t2)))
            else Right "Error: Information flow violation in lambda"
        _ -> Right "Error: Type mismatch in lambda"


typeCheck g (App e1 e2) = 
    case (typeCheck g e1, typeCheck g e2) of
        (Left (Lab (Arr t1' t2') label1), Left t2)
            | t1' == t2 ->
                if leq (labelOf t2) label1 
                then Left (Lab (typeOf t2') (join (labelOf t2') label1))
                else Right "Error: Information flow violation in application"
            | otherwise -> Right "Error: Type mismatch in application"
        (_, _) -> Right "Error: Type error in application"
  where
    typeOf (Lab t _) = t
