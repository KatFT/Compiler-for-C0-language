module CMips where

import Interm
import Parser
import Data.Map (Map)
import qualified Data.Map as Map

-- function that receives the intermediate code and translates to mips
mips :: [Instr] -> IO ()
mips instrs
  = do [Func f args stmts] <- return (dealMain instrs)               -- gets the main function
       let fun = [Func f args stmts] in do
         if (fun /= [])                                              -- if main function exists in the program
           then do transText [START]                                 -- start printing the header of a mips code
                   transText fun  
                   putStrLn ("\tli $v0, 10" ++ "\n\tsyscall")                                   -- print the main function
                   checkDefinedFunc stmts                            -- prints the print_int/scan_int mips code if they exist
                   transText (removeMain instrs)                     -- prints the rest of the functions besides main
           else do error "There isn't a function main defined"

dealMain :: [Instr] -> [Instr]
dealMain [] = []
dealMain ((Func f args stmts):rest) = do
    if(f == "main")
      then do [Func f args (removeReturnMain stmts)]
      else do (dealMain rest)                         -- continues searching for main

-- function that removes the main function from the list after being processed
-- so that the rest of the functions can be processed
removeMain :: [Instr] -> [Instr]
removeMain ((Func f args stmts):rest) =
  let fun = Func f args stmts in
    if (f == "main") then rest
    else (fun : (removeMain rest)) 

removeReturnMain ::[Instr] -> [Instr]
removeReturnMain [] = error "return is not defined"
removeReturnMain (x:xs) = 
  case x of 
    (RETURN t) -> xs
    _ -> x : removeReturnMain xs 

-- if we encounter a print_int or scan_int call we print the instructions of the function 
checkDefinedFunc :: [Instr] -> IO ()
checkDefinedFunc [] = return ()
checkDefinedFunc (x:xs) = do
  case x of
    (PRINTINT t) -> do putStrLn ("print_int:")
                       putStrLn ("\tli $v0, 1")
                       putStrLn ("\tmove $a0, $" ++ t )
                       putStrLn ("\tsyscall")
                       putStrLn ("\tjr $ra")
                       checkDefinedFunc xs
                    
    (CALL _ "scan_int" _) -> do putStrLn ("scan_int:")
                                putStrLn ("\tli $v0, 5")
                                putStrLn ("\tsyscall")
                                putStrLn ("\tjr $ra")
                                checkDefinedFunc xs

    _ -> checkDefinedFunc xs

-- function that will process the instructions received

{-
   transText : the second argument will be ignored since this case corresponds to the last
   instruction and the second argument is only taken into account when we have a COND case
   and it is guaranteed that there will always be more instructions after a COND instruction
-}

transText :: [Instr] -> IO ()

transText [] = return ()

transText [instr] -- base case
  = printText instr instr

transText (instr:next:instrs)
  = do printText instr next 
       transText (next:instrs)

-- function that will print EACH instruction according to its content
printText :: Instr -> Instr -> IO ()

printText BREAK _ = return ()

printText CONTINUE _ = return ()

printText (JUMP label) _ = putStrLn ("\tj " ++ label)

printText START _ = putStrLn ("\t.text\n") 

printText (MOVE t1 t2) _ = putStrLn ("\tmove $" ++ t1 ++ ", $" ++ t2)

printText (MOVEI t i) _ = putStrLn ("\tli $" ++ t ++ ", " ++ (show i))

printText (OP op t2 t0 t1) _
  = case op of
      Plus -> putStrLn ("\tadd $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      Minus -> putStrLn ("\tsub $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      Div -> putStrLn ("\tdiv $" ++ t0 ++ ", $" ++ t1 ++ "\n\t mflo $" ++ t2)
      Times -> putStrLn ("\tmul $" ++ t2 ++ ", $" ++ t0 ++ ", $" ++ t1)
      Mod -> putStrLn ("\tdiv $"++t0++", $"++t1 ++"\n\tmfhi $"++t2)

printText (OPI op t1 t0 i) _
  = case op of
      Plus -> putStrLn ("\taddi $" ++ t1 ++ ", $" ++ t0 ++ ", " ++ (show i))
      _ -> return ()

printText (LABEL l1) _ = putStrLn (l1 ++ ": ")


printText (COND t1 op t2 l1 l2) (LABEL l)
  | l == l2 = putStrLn ("\t" ++ (opMips "second" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1)
  | l == l1 = putStrLn ("\t" ++ (opMips "first" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l2)

-- if we dont have a label after the condition instruction
printText (COND t1 op t2 l1 l2) _
  = putStrLn ("\t" ++ (opMips "second" op) ++ " $" ++ t1 ++ ", $" ++ t2 ++ ", " ++ l1 ++ "\nj " ++ l2)
                         
printText (RETURN t) _ 
  = putStrLn ("\tmove $v0, $" ++ t ++ "\n\tla $sp, 0($fp)" ++
              "\n\tlw $ra, -8($sp)" ++ "\n\tlw $fp, -4($sp)" ++ "\n\tjr $ra")

printText (PRINTINT dest) _ =
  do putStrLn ("\tla $sp, -4($sp)")                                         -- grow stack
     putStrLn ("\tjal print_int")                                           -- jump and link
     putStrLn ("\tla $sp, 4($sp)")                                          -- shrink stack
     putStrLn ("\tmove $" ++ dest ++ ", $v0")                                -- save result
              
printText (CALL dest f argList) _ = do
  if (f == "scan_int")
    then do putStrLn ("\tjal scan_int")
            putStrLn ("\tmove $" ++ dest ++ ", $v0") --adicionei esta linha para armazenar o nosso "input" qnd chega do scan
              
    else do printStores (reverse argList) 1
            putStrLn ("\tla $sp, " ++ (show ((-4)*(length argList))) ++ "($sp)")   -- grow stack
            putStrLn ("\tjal " ++ f)                                               -- jump and link
            putStrLn ("\tla $sp, " ++ (show (4*(length argList))) ++ "($sp)")      -- shrink stack
            putStrLn ("\tmove $" ++ dest ++ ", $v0")                                -- save result

printText (Func fun(argList) funCode) _ =
  do putStrLn (fun ++ ":")                                                 -- entry label for fun
     putStrLn ("\tsw $fp, -4($sp)")                                        -- save old $fp
     putStrLn ("\tsw $ra, -8($sp)")                                        -- save return address
     putStrLn ("\tla $fp, 0($sp)")                                         -- setup frame pointer
     putStrLn ("\tla $sp, " ++ (show (length argList * (-4)))  ++ "($sp)") -- allocate frame
     loadArgs argList 0
     transText funCode

-- loads the args of the function and allocates them on the frame
loadArgs :: ArgsList -> Integer -> IO ()
loadArgs [] _ = return ()
loadArgs (a:as) n =
  do putStrLn ("\tlw $" ++ a ++ ", " ++ (show n) ++ "($fp)")
     loadArgs as (n+4)

-- store args
printStores :: [Temp] -> Int -> IO ()
printStores [] _ = return ()
printStores (arg:args) n
  = do putStrLn ("\tsw $" ++ arg ++ ", " ++ (show (-4 * n)) ++ "($sp)")
       printStores args (n+1)

-- translation of the conditions depending on the
-- the instruction that comes next: ltrue, lfalse or neither
opMips :: String -> RelOp -> String
opMips "first" op
  | op == Lt = "bge"
  | op == Lteq = "bgt"
  | op == Gt = "ble"
  | op == Gteq = "blt"
  | op == Diff = "beq"
  | otherwise = "bne"

opMips "second" op
  | op == Lt = "blt"
  | op == Lteq = "ble"
  | op == Gt = "bgt"
  | op == Gteq = "bge"
  | op == Diff = "bne"
  | otherwise = "beq"