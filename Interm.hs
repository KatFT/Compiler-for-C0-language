module Interm where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Temp = String
type Label = String
type Arg = String
type Function = String

type ArgsList = [Arg]

data Instr = MOVE Temp Temp                       -- temp1 := temp2
           | MOVEI Temp Int                       -- temp1 := num
           | OP BinOp Temp Temp Temp              -- temp1 := temp2 op temp3
           | OPI BinOp Temp Temp Int              -- temp1 := temp2 op num
           | LABEL Label
           | JUMP Label 
           | COND Temp RelOp Temp Label Label
           | CALL Temp Label [Temp]
           | RETURN Temp
           | BREAK
           | CONTINUE
           | Func Ident(ArgsList) [Instr]
           | START
           | PRINTINT Temp
           deriving (Eq, Show)

type Table = Map Ident String

type Count = (Int, Int, Int, Int)

-- function that receives the AST and starts the conversion to intermediate code
interm :: Table -> [Fun] -> State Count [Instr]
interm table [] = return []
interm table ((FunDef flabel tp args instr):funs) =
  let fun = FunDef flabel tp args instr in do
    -- put all the functions definitions on the table if it's the first time we deal with the table
    -- if not then the table remains the same
    table <- if(Map.null (table)) then (getAllFun table (fun:funs)) else (return table)
    -- update the table with all the variable declarations made on the function fun
    tab <- getAllVal table instr
    -- reset temps count used in all functions besides transExp
    -- the total elements on tab minus the nr of function definitions = nr temps in current function
    popTemp(Map.size tab - Map.size table)
    -- translate all the instructions in the function
    result <- transFun (fun, tab)
    -- do all the above for the rest of the functions contained in funs given only the table
    -- with the functions' definitions
    rest <- interm table funs
    return (result:rest)

-- goes through the AST and inserts all function definitions on the table
getAllFun :: Table -> [Fun] -> State Count Table
getAllFun table [] = return table
getAllFun table (fun:funs)
  = do tab <- insertFun table fun
       rest <- getAllFun tab funs
       return rest

insertFun :: Table -> Fun -> State Count Table
insertFun table (FunDef flabel tp args instr)
  = do fun <- newFun
       return (Map.insert flabel fun table)

insertVal :: Table -> Stmt -> State Count Table
insertVal table (Decl (var, _))
  = do temp <- newTemp
       return (Map.insert var temp table)

-- goes through the block a function's instructions' block and inserts all variable definitions on the table
getAllVal :: Table -> [Stmt] -> State Count Table
getAllVal table [] = return table
getAllVal table (stmt:stmts)
  = case stmt of
      (Decl dec) -> do tab <- insertVal table (Decl dec)
                       getAllVal tab stmts
      _ -> getAllVal table stmts

insertArg :: Table -> (Ident, Type) -> State Count (Arg, Table)
insertArg table (arg, _) = do a <- newArg
                              return (a, Map.insert arg a table)
             
-- create a new temporary variable
newTemp :: State Count Temp
newTemp = do (temps,labels,args,funcs) <- get
             put (temps+1,labels,args,funcs)
             return ("t"++show temps)

-- reuse temporary variables
popTemp :: Int -> State Count ()
popTemp k = do (temps,labels,args,funcs) <- get
               put (temps-k,labels,args,funcs)

-- reuse arguments after a function's definition
popArg :: Int -> State Count ()
popArg k = do (temps,labels,args,funcs) <- get
              put (temps,labels,args-k,funcs)

-- create a new label
newLabel :: State Count Label
newLabel = do (temps,labels,args,funcs) <- get
              put (temps,labels+1,args,funcs)
              return ("L"++show labels)

-- create a new function's argument
newArg :: State Count Arg
newArg = do (temps,labels,args,funcs) <- get
            put (temps,labels,args+1,funcs)
            return ("a"++show args)

-- create a new variable that will represent the function
newFun :: State Count Function
newFun = do (temps,labels,args,funcs) <- get
            put (temps,labels,args,funcs+1)
            return ("f"++show funcs)

----------------------------- FUNCTION DEFINITION ----------------------------------

-- translate a function
transFun :: (Fun, Table) -> State Count Instr
transFun (FunDef f typ argList stmt, table)
  = do (args, tabl) <- argTemp argList table
       popArg(length args)
       stmts <- transFunStmt tabl stmt
       return (Func f args stmts)

-- translates the function's instructions' block
transFunStmt :: Table -> [Stmt] -> State Count [Instr]
transFunStmt table [] = return []
transFunStmt table (stmt:stmts)
  = do code <- transStm table stmt
       listStmt <- transFunStmt table stmts
       return (code ++ listStmt)
       
-- returns list of temporary variables that represent the arguments of the function
argTemp :: ArgList -> Table ->  State Count ([Temp], Table)
argTemp [] table = return ([], table)
argTemp (arg:args) table
  = do (a, tab) <- insertArg table arg
       (as, tabl) <- argTemp args tab
       return ((a:as), tabl)

-------------------------------- EXPRESSIONS ----------------------------------------

-- translate an expression
transExp :: (Exp, Table, Temp) -> State Count [Instr]

transExp (Num n, table, dest)
  = return [MOVEI dest n]

transExp (Var x, table, dest)
  = case Map.lookup x table of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExp (BoolVal b, table, dest)
  = case b of
      ValTrue -> return [MOVEI dest 1]
      ValFalse -> return [MOVEI dest 0]

transExp (OpBin op e1 e2, table, dest)
  = do case e1 of
         Num x -> do t2 <- newTemp
                     code2 <- transExp(e2, table, t2)
                     popTemp(1)
                     return (code2 ++ [OPI op dest t2 x])
         _ -> case e2 of
                Num x -> do t1 <- newTemp
                            code1 <- transExp(e1, table, t1)
                            popTemp(1)
                            return (code1 ++ [OPI op dest t1 x])
                _ -> do t1 <- newTemp
                        t2 <- newTemp
                        code1 <- transExp(e1, table, t1)
                        code2 <- transExp(e2, table, t2)
                        popTemp(2)
                        return (code1 ++ code2 ++ [OP op dest t1 t2])

transExp (OpRel op e1 e2, table, dest)
  = do label1 <- newLabel
       label2 <- newLabel
       code <- transCond(OpRel op e1 e2, table, label1, label2)
       -- we start by initializing the result to 0
       -- if the condition is true then we change the result to 1
       -- else it remains 0
       return ([MOVEI dest 0] ++ code ++ [LABEL label1, MOVEI dest 1]
         ++ [LABEL label2])

transExp (FunCall f args, table, dest)
  -- there is no need to check if this functions are on the table since they are already defined
  = do if (f == "scan_int")
         then do if(length args /= 0)
                   then do error "Function scan_int can't have arguments"
                   else do (code, temps) <- transExps(table, args)
                           popTemp(length temps)
                           return (code ++ [CALL dest f temps])
                 
         else do case Map.lookup f table of
                   Nothing -> error "undefined function"
                   Just flabel -> do (code, temps) <- transExps(table, args)
                                     return (code ++ [CALL dest f temps])

-- função auxiliar que retorna a tradução de cada um dos argumentos da função
-- e a lista dos temporários da função
transExps :: (Table, [Exp]) -> State Count ([Instr], [Temp])
transExps (table, []) = return ([], [])
transExps (table, (exp:exps))
  =  do temp <- newTemp
        code <- transExp(exp, table, temp)
        --popTemp(1)
        (code', temps) <- transExps (table, exps)
        popTemp(length temps)
        return (code ++ code', temp:temps)

-- translate a condition
transCond :: (Exp, Table, Label, Label) -> State Count [Instr]

transCond (cond, table, ltrue, lfalse)
  = case cond of
      BoolVal ValTrue -> return [JUMP ltrue]
      BoolVal ValFalse -> return [JUMP lfalse]
      
      Not exp -> transCond(exp, table, lfalse, ltrue)
                
      OpRel op exp1 exp2 -> case op of
        And ->
          do lnext <- newLabel
             code1 <- transCond(exp1, table, lnext, lfalse)
             code2 <- transCond(exp2, table, ltrue, lfalse)
             return (code1 ++ [LABEL lnext] ++ code2)
           
        Or ->
          do lnext <- newLabel
             code1 <- transCond(exp1, table, ltrue, lnext)
             code2 <- transCond(exp1, table, ltrue, lfalse)
             return (code1 ++ [LABEL lnext] ++ code2)
           
        otherwise ->
          do temp1 <- newTemp
             temp2 <- newTemp
             popTemp(2)
             code1 <- transExp(exp1, table, temp1)
             code2 <- transExp(exp2, table, temp2)
             return (code1 ++ code2 ++ [COND temp1 op temp2 ltrue lfalse])

      -- otherwise it's another type of expression
      exp -> do temp <- newTemp
                popTemp(1)
                code1 <- transExp(exp, table, temp)
                return (code1 ++ [COND temp Diff "0" ltrue lfalse])

-- translate a statement
transStm :: Table -> Stmt -> State Count [Instr]

transStm table Skip = return []

transStm table (Decl decl) = return []

transStm table (PrintINT e)
  = do dest <- newTemp
       code <- transExp(e, table, dest)
       return (code ++ [PRINTINT dest])

transStm table (Block stmts) = transBlock table stmts
  where
    transBlock table [] = return []
    transBlock table (stmt:stmts)
      = do code1 <- transStm table stmt
           code2 <- transBlock table stmts
           return (code1 ++ code2)

transStm table (Return exp)
  = do temp <- newTemp
       code <- transExp(exp, table, temp)
       return (code ++ [RETURN temp])

transStm table (Assign var exp)
  = case Map.lookup var table of
      Nothing -> error "undefined variable"
      Just dest -> do temp <- newTemp
                      code <- transExp(exp, table, temp)
                      return (code ++ [MOVE dest temp])

transStm table (If cond stm1)
  = do ltrue <- newLabel
       lfalse <- newLabel
       code0 <- transCond(cond, table, ltrue, lfalse)
       code1 <- transStm table stm1
       return (code0 ++ [LABEL ltrue] ++ code1 ++ [LABEL lfalse])

transStm table (IfElse cond stm1 stm2)
  = do ltrue <- newLabel
       lfalse <- newLabel
       lend <- newLabel
       code0 <- transCond(cond, table, ltrue, lfalse)
       code1 <- transStm table stm1
       code2 <- transStm table stm2
       return (code0 ++ [LABEL ltrue] ++ code1 ++
              [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])

transStm table (While cond stm)
  = do loop <- newLabel
       lnext <- newLabel
       lend <- newLabel
       code1 <- transCond(cond, table, lnext, lend)
       code2 <- transStm table stm
       code3 <- breakStm code2 lend 
       code4 <- continueStm code3 loop
       return ([LABEL loop] ++ code1 ++ [LABEL lnext] ++
         code4 ++ [JUMP loop, LABEL lend])

transStm table (For init cond stm1 stm2)
  = do loop <- newLabel
       lnext <- newLabel
       lend <- newLabel
       code1 <- transStm table init
       code2 <- transCond(cond, table, lnext, lend)
       code3 <- transStm table stm1
       code4 <- transStm table stm2
       code5 <- breakStm code4 lend 
       code6 <- continueStm code5 loop
       return (code1 ++ [LABEL loop] ++ code2 ++ [LABEL lnext] ++ code6 ++ code3 ++ [JUMP loop, LABEL lend])

transStm table Break = return [BREAK]

transStm table Continue = return [CONTINUE]

transStm table (Incr var)
  = case Map.lookup var table of
      Nothing -> error "undefined variable"
      Just dest -> return [OPI Plus dest dest 1]

transStm table (Decr var)
  = case Map.lookup var table of
      Nothing -> error "undefined variable"
      Just dest -> return [OPI Minus dest dest 1]

breakStm :: [Instr] -> Label -> State Count [Instr]
breakStm code lend
  | BREAK `elem` code = replace BREAK (JUMP lend) code
  | otherwise = return code

continueStm :: [Instr] -> Label -> State Count [Instr]
continueStm code loop
  | CONTINUE `elem` code = replace CONTINUE (JUMP loop) code
  | otherwise = return code

replace :: Instr -> Instr -> [Instr] -> State Count [Instr]
replace _ _ [] = return []
replace stm1 stm2 (x:xs)
  | stm1 == x =
      do rest <- replace stm1 stm2 xs
         return (stm2 : rest)
  | otherwise =
      do rest <- replace stm1 stm2 xs
         return (x : rest)
