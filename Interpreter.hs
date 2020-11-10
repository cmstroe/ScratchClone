--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
-- for the assignment statement we first look if the variable is already in memory or not;
-- If it's not already in memory, we get the value of the expression and add the pair (var, result of expr)
-- to memory as a (string, Int) pair with the location being the initial string var
-- If it already is in memory, it will be deleted from the present location and then added to the head of the list.
-- the lookup function used find whether or not a key is in a list or returns Nothing
interpret ((AssignStmt var expr):xs) mem
                                    | lookup var mem == Nothing = do aux <- retrieveexpr expr mem
                                                                     (interpret xs ((var, aux):mem))
                                    | otherwise = do aux <- retrieveexpr expr mem
                                                     interpret xs ((var, aux):(deletefromlist var [] mem))


-- The if statement takes 4 parameters: the first is the condition of the statement which is one Expression,
-- the second is the body which contains multuiple statements, the third is an else if which contains both another condition and a set of statements
-- the fourth is the final else that also contains a set of statements; each condition is tested using the test function
-- this function decides if the condition is valid and executes the inner statements; The list of if else statements is concatenated
-- to the initial if and the if body using the '++' operator. So is the final else, but for the final else, this will be executed as a default
-- so its truth value will be always 1 even though it doesn't contain a condition
--
interpret ((IfStmt cond stmt iei ie):xs) mem = test ([(cond,stmt)] ++ (iei) ++ [(ValE 1,ie)]) mem

-- the repeat steatement had 2 parameters : the number of times to be repeted and the body of the
-- function which contains the instructions; first, store the number of times the instructions still need to be executed
-- then, if the number is 0, interpret the last set of instructions. If there still are  a number of repetitions
-- to be done, execute the statement once, add it to the head of the memory list and then interpret the rest with one less
-- repetition to be done and with the memory including the last occurence of the statement.


interpret ((RepeatStmt expr stmt):xs) mem = do aux <- retrieveexpr expr mem
                                               if (aux <= 0 ) then interpret xs mem
                                               else do newmemory <- interpret stmt mem
                                                       interpret ((RepeatStmt (ValE (aux-1)) stmt):xs) newmemory

-- functions to retrive expressions from memory : the expression xan be of 3 sorts
-- ValE, Var E and BinOpE, but the latter also contains two expressions and an operand
-- each of the functionstreats one carticular case, startinf grom the simplest to the most
-- complex;
retrieveexpr :: Expr -> Memory -> Either Err Int

-- if is is just an int value store it
retrieveexpr (ValE x) mem = Right x

-- the variables are strings : in memory named locations map to values; if the location does not exist in the
-- memory, return the UninitialisedMemory error.

retrieveexpr (VarE x) mem = maybefunction (UninitialisedMemory x) (lookup x mem)

retrieveexpr (BinOpE op (ValE x) (ValE y)) mem = case op of
                                                      Add -> Right (x+y)
                                                      Sub -> Right (x-y)
                                                      Mul -> Right (x*y)
                                                      Div -> if (y == 0) then Left DivByZeroError else Right (div x y)
                                                      Pow -> if (y < 0)  then Left NegativeExponentError else Right (x^y)
                                                      Equal -> if (x == y) then Right 1 else Right 0
                                                      Neq   -> if (x == y ) then Right 0 else Right 1
                                                      LessThan -> if (x < y ) then Right 1 else Right 0
                                                      LessOrEqual -> if (x <= y) then Right 1 else Right 0
                                                      GreaterThan -> if ( x > y ) then Right 1 else Right 0
                                                      GreaterOrEqual -> if (x >= y) then Right 1 else Right 0
--if only one of the factors in the operation need to be retrieved look for its value in memory
-- then use the function with identified expressions to calculate the value

retrieveexpr (BinOpE op (ValE x) y) mem = do aux <- retrieveexpr y mem
                                             retrieveexpr ( BinOpE op (ValE x) (ValE aux)) mem

--  when bothexpressions are unknown find out the first one and use the
-- above function for the second one
retrieveexpr (BinOpE op x y) mem = do aux <- retrieveexpr x mem
                                      retrieveexpr (BinOpE op (ValE aux) y) mem


-- takes a maybe value and return it as a either: if error do left error,
-- if valid value do right value
maybefunction :: a -> Maybe b -> Either a b
maybefunction error = maybe (Left error) (\x -> Right x)

-- the test function takes the expressions that condition the if and sees if it is true∷
-- if it is true, the statement conditionet by it has to be interpreted and added to the Memory
-- if not, it has to move to the next if in the list and evaluate the next expression
test ::[(Expr,[Stmt])] -> Memory -> Either Err Memory
test ((expr,stmt):xs) mem = do aux <- retrieveexpr expr mem
                               if (aux == 0) then test xs mem
                               else  interpret stmt mem

-- delete the spaces in memory that are not useful anymore
-- while the string is not the head element of the memory list,
-- delete the elements in the memory by concatenating them to z which will be
-- the empty list at first; if s is found somewhere in memory, the two lists are concatenated and
-- s is omitted from the new list created that hass all the other initial elements of x:xs

deletefromlist :: String -> Memory -> Memory -> Memory
deletefromlist s z ((x,y):xs)
              | x == s = z ++ xs
              | otherwise = deletefromlist s ((x,y):z) xs
