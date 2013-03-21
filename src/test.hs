import Data.Monoid
import Control.Monad


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


-- infinitely tall char blocks,
-- repeating after n lines
-- whose first line represents the given open vars.
data Dia = Dia { openVars :: [String]
               , height :: Int
               , table :: [[Char]]
               }

instance Monoid Dia where
  mempty = Dia [] 0 []
  mappend (Dia v1 h1 t1) (Dia v2 h2 t2) = Dia (v1 ++ v2)
                                              (max h1 h2)
                                              (zipWith (++) t1 t2)

white :: Dia
white = Dia [""] 0 $ repeat " "

black :: String -> Dia
black s = Dia ["", s, ""] 1 $ repeat " * "

print_dia :: String -> Dia -> IO ()
print_dia indent (Dia ss h xxs) = do putStr indent
                                     forM_ ss $ \s -> do
                                       if s == ""
                                       then putStr " "
                                       else putStr s
                                     putStrLn ""
                                     forM_ (take (h+1) xxs) $ \xs -> do
                                       putStrLn $ indent ++ xs

dia :: Exp -> Dia
dia (Lam s x) = Dia ss' h' (xs':row:xs:xxs) where
  Dia ss h (xs:xxs) = dia x
  ss' = map drop_s ss
  h' = h + 2
  xs' = zipWith drop_s' ss xs
  row = map (const '*') xs
  drop_s  s2   = if s2 == s then "" else s2
  drop_s' s2 x = if s2 == s then ' ' else x
dia (Var s) = black s
dia (App x y) = Dia ss h' xxs' where
  Dia ss h xxs = dia x `mappend` white `mappend` dia y
  w = length ss
  h' = h + 2
  xs = xxs !! h
  (xs', _) = foldr connect ([], False) xs
  connect '*' (xs, force_stars) = ('*':xs, not force_stars)
  connect  _  (xs, True)        = ('*':xs, True)
  connect  x  (xs, False)       = ( x :xs, False)
  xs'' = take w $ takeWhile (== ' ') xs ++ "*" ++ repeat ' '
  xxs' = take h xxs ++ [xs'] ++ repeat xs''


main = forM_ exps $ \(name, s) -> do
  let e = read s :: Exp
  putStrLn $ name ++ "\t" ++ show e
  print_dia "    " $ dia e
