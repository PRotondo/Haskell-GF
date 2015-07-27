{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
import GF
import Language.Haskell.TH

type C = CombExp

-- generate code for the system of x0, x1 and x2
-- {x0 = Set x1, x1 = Cycle x2, x2 = Atom x Set x2}
gen (\(x0 :: C) (x1 :: C) (x2:: C) -> [ Set x1, Cycle x2, Prod Atom (Set x2)])

-- Resulting variables always get named x0 x1 x2 x3 ... and so on in the order
-- they appear in your lambda expression.

main = do
    runQ(gen (\(x0 :: C) (x1 :: C) (x2:: C) -> [ Set x1, Cycle x2, Prod Atom (Set x2)])) >>= putStrLn.pprint
    putStrLn(show( (( x0) !! 200) :: Rational ) )
