import Control.Monad
import Control.Monad.Writer


data Exp = Lam String Exp
         | Var String
         | App Exp Exp
         deriving Eq


readsSpaces :: ReadS ()
readsSpaces xs = [((), dropWhile (==' ') xs)]

readsChars :: String -> ReadS String
readsChars alphabet xs = if null word then [] else [(word, xs')] where
  word = takeWhile (`elem` alphabet) xs
  xs' = dropWhile (`elem` alphabet) xs

instance Read Exp where
  readsPrec d r = readParen (d > lam_prec)
                  (\r0 -> [(Lam s x, r4) |
                           ('λ': r1) <- [r0],
                           (s,   r2) <- lex r1,
                           (".", r3) <- lex r2,
                           (x,   r4) <- readsPrec lam_prec r3]) r
               ++ [(Var s, r') |
                   (s, r') <- readsChars ['a'..'z'] r]
               ++ readParen (d > app_prec)
                  (\r0 -> [(App x y, r3) |
                           (x, r1) <- readsPrec (app_prec+1) r0,
                           ((), r2) <- readsSpaces r1,
                           (y, r3) <- readsPrec (app_prec+1) r2]) r
               where
    lam_prec = 5
    app_prec = 10


showOptionalSpace :: ShowS
showOptionalSpace ('(':xs) = '(':xs
showOptionalSpace xs = ' ':xs

instance Show Exp where
  showsPrec d (Lam s x) = showParen (d > lam_prec) $
                            showString "λ" . showString s . showString "." . showsPrec lam_prec x
                          where lam_prec = 5
  showsPrec d (Var s) = showString s
  showsPrec d (App x y) = showParen (d > app_prec) $
                            showsPrec (app_prec+1) x . showOptionalSpace . showsPrec (app_prec+1) y
                          where app_prec = 10

exps = [("id",     "λx.x"),
        ("K/true", "λx.λy.x"),
        ("false",  "λx.λy.y"),
        ("S",      "λx.λy.λz.(x z)(y z)"),
        ("Y",      "λf.(λx.x x)(λx.f(x x))"),
        ("2",      "λf.λx.f(f x)"),
        ("3",      "λf.λx.f(f(f x))"),
        ("4",      "λf.λx.f(f(f(f x)))"),
        ("Ω",      "(λx.x x)(λx.x x)"),
        ("chrisdoner", "λn.λf.λx.((n(λg.λh.h(g f)))(λu.x))(λu.u)")]


type C_Program = [String]

compile :: Exp -> C_Program
compile e = execWriter $ do c <- eval e
                            write $ "T main = " ++ c ++ ";"
            where
  eval :: Exp -> Writer [String] String
  eval _ = do write "T anonymous1(T x) {"
              write "  return x;"
              write "}"
              return "anonymous1"
  
  write x = tell [x]


print_indented :: String -> [String] -> IO ()
print_indented indent = mapM_ putStrLn . map (indent++)

main = forM_ exps $ \(name, s) -> do
  let e = read s :: Exp
  putStrLn $ name ++ "\t" ++ show e
  print_indented "    " $ compile e
  putStrLn ""
