import qualified Data.Map as Map
import System.IO
import Data.Maybe
import System.IO.Unsafe
import Control.Exception

type NaiveStore = Map.Map String Value

data Value =
       VInt Int
     | VFun Stmt
     deriving (Show)

data Expr =
       CstI Int
     | Var String
     | VarArr(String, Expr)                -- VarArray(name, index)
     | Prim(String, Expr, Expr)
     | Call(String, [(String, Expr)])      -- Call(function name, [(arg name, arg value)])
     deriving (Show, Read)

data Stmt =
       Asgn(String, Expr)
     | AsgnArr(String, Expr, Expr)
     | InitArr(String, Expr, Expr)         -- Array(name, size, init value)
     | If(Expr, Stmt, Stmt)
     | Block [Stmt]
     | For(String, Expr, Expr, Stmt)
     | While(Expr, Stmt)
     | Func(String, Stmt)                  -- Func(function name, function body)
     | Return Expr
     | Print Expr
     deriving (Show, Read)

eval :: Expr -> NaiveStore -> Int
eval expr store =
     case expr of CstI i -> i
                  Var x -> case getStore store x of VInt y -> y
                                                    _ -> error "This is a function."
                  VarArr(i, e) -> 
                   case getStore store $ arrName i $ eval e store of VInt x -> x
                                                                     _ -> error "This is a function."
                  Prim(ope, e1, e2)
                   | ope == "*" -> i1 * i2
                   | ope == "/" -> i1 `div` i2
                   | ope == "+" -> i1 + i2
                   | ope == "-" -> i1 - i2
                   | ope == "==" -> if i1 == i2 then 1 else 0
                   | ope == "<" -> if i1 < i2 then 1 else 0
                   | otherwise -> error "unknown primitive"
                    where i1 = eval e1 store
                          i2 = eval e2 store
                  Call(f, earg) -> case getStore resstore retname of VInt x -> x
                   where arg = map (\(argn, argv) -> (argn, VInt $ eval argv store)) earg
                         argstore = foldl setStore emptyStore arg
                         func = case getStore store f of VFun vf -> vf
                         resstore = exec func argstore

exec :: Stmt -> NaiveStore -> NaiveStore
exec stmt store =
     case stmt of Asgn(x, e) -> eval e store `seq` setStore store (x, VInt $ eval e store)
                  AsgnArr(x, eindex, e) -> eval e store `seq` setStore store (arrName x index, VInt $ eval e store)
                   where index = eval eindex store
                  InitArr(x, esz, eini) -> loop x sz ini store
                   where sz = (eval esz store) - 1
                         ini = eval eini store
                         loop v s i sto = if s == -1
                                          then sto
                                          else loop x (s-1) i (setStore sto (arrName v s, VInt i))
                  If(e, stmt1, stmt2) -> if eval e store /= 0 then exec stmt1 store
                                                              else exec stmt2 store
                  Block stmts -> foldl (flip exec) store stmts
                  For(x, estart, estop, stmt) -> loop start store
                   where start = eval estart store
                         stop = eval estop store
                         loop i sto = if i > stop
                                      then sto
                                      else loop (i+1) (exec stmt (setStore sto (x, VInt i)))
                  While(e, stmt) -> loop store
                   where loop sto = if eval e sto == 0 then sto
                                                       else loop (exec stmt sto)
                  Func(f, stmts) -> setStore store (f, VFun stmts)
                  Return e -> setStore store (retname, VInt $ eval e store)
                  Print e -> unsafePerformIO $ do{print (eval e store); return store}

getStore :: NaiveStore -> String -> Value
getStore store x = fromJust $ Map.lookup x store

setStore :: NaiveStore -> (String, Value) -> NaiveStore
setStore store (k, v) = Map.insert k v store

arrName :: String -> Int -> String
arrName s i = s ++ "[" ++ (show i) ++ "]"

prompt :: String -> IO ()
prompt p = putStr p >> hFlush stdout

emptyStore = Map.empty

retname = "-ret-"

run :: NaiveStore -> IO ()
run store = (do
            prompt "ipretC> "
            input <- getLine
            let res = (exec (read input) store)
            if null input then run store else run $! res)
            `catches` [Handler ioException, Handler $ ignore store]

ignore :: NaiveStore -> SomeException -> IO ()
ignore store excp = do{print excp; run $! store}

ioException :: IOException -> IO ()
ioException _ = return ()

main = run emptyStore