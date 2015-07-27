{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
module GF(CombExp(..),CombSys(gen)) where
import Language.Haskell.TH

data CombExp = Prod (CombExp) CombExp
         | Union (CombExp) CombExp
         | Atom
         | Epsilon
         | Var String
         | Set CombExp
         | Cycle CombExp
         | Seq CombExp
--         Comp g h = substitue z for g in h
         | Comp (CombExp) (CombExp)
         deriving (Show,Eq,Ord)

sqr (x:xs) = (x*x) : ( ( map ((*2).(*x)) xs) + (0 : sqr xs) )

constD :: Num a => a -> [a]
constD x	=	x: repeat 0

instance (Eq a, Num a) => Num [a] where
	fromInteger x			=	constD (fromInteger x)
	(+)				=	zipWith (+)
	xs@(x:x')	*  ys@(y:y') 	=	case x of
							0 -> 0 : (x'*ys)
							_ -> case y of
								0 -> 0 : (xs*y')
								_ -> (x*y) : ( ( map (*x) y' + map (*y) x') + (0 : (x' * y')) )
	negate 				=	map negate
	signum _			=	error "The function signum is not defined for GFs"
	abs _				=	error "The function abs is not defined for GFs"

instance (Eq x, Fractional x) => Fractional [x] where
	fromRational x	=	constD (fromRational x)
	recip (x:xs)	=	case x of
					0 -> error "No Multiplicative Inverse"
					_ -> let ans = (1 / x) : map (/x) (negate xs * ans)  in ans
	(/) a@(x:xs) b@(y:ys) = if x == 0 && y == 0 then xs / ys
				else ( if y == 0 then error "Division by 0" else a * recip b)

d :: (Eq x, Enum x, Num x) => [x] -> [x]
d (f:fs) = zipWith (*) [1..] fs

i :: (Eq x, Enum x, Fractional x) => [x] -> [x]
i f = zipWith (/) f [1..]

int :: (Eq x, Enum x, Fractional x) => [x] -> [x]
int f = 0 : i f

instance (Eq x, Enum x,Fractional x) => Floating [x] where
	pi		=	error "The constant pi is not defined for GFs"
	exp 	f	=	let f' = d f in (let ans = 1  :  i ( ans * f' ) in ans)
	log 	f	=	0  :  i ( d f  / f)
	sqrt 	f	=	let f' = d f in (let ans = 1 :  zipWith (flip (/) ) [fromInteger (2*i) | i <- [1..]]  ( f'  / ans ) in ans)
	asin 	f	=	0 :  i  ( d f / sqrt ( 1 - sqr f )   )
	acos 	f	=	1 :  negate ( i ( d f / sqrt ( 1 - sqr f )  ) )
	atan 	f	=	0 :  i  ( d f / (1 + sqr f)   )
	sinh 	f	=	map (/2) $ exp f - exp ( negate f)
	cosh 	f	=	map (/2) $ exp f + exp ( negate f)
	asinh 	f	=	0  : i (  d f /  sqrt ( 1 + sqr f) )
	acosh 	f	=	1  : i (  d f /  sqrt ( sqr f - 1)  )
	atanh 	f	=	0  : i (  d f /  (1 -  sqr f )  )
	sin	f	=	let f' = d f in (let (x,y) = (1 :  i ( negate y * f')  ,0 :  i ( x * f') )  in	y)
	cos	f	=	let f' = d f in (let (x,y) = (1 :  i ( negate y * f')  ,0 :  i ( x * f') )  in	x)

compose :: (Num a,Eq a) => [a] -> [a] -> [a]
compose (f@(f0:f1)) (g@(0:g1)) = f0 : (g1 * compose f1 g)

z :: (Eq a, Num a) => [a]
z = 0:1

factorial = let ys = 1 : zipWith (*) [1..] ys in ys

translate :: CombExp -> ExpQ
translate Epsilon = [|1|]
translate (Atom) = [|z|]
translate (Prod b Epsilon) = translate b
translate (Prod Epsilon b) = translate b
translate (Prod b Atom) = [| (0 : $(translate b) ) |]
translate (Prod Atom b) = [| (0 : $(translate b) ) |]
translate (Prod a b) = [|  ($(translate a) * $(translate b)) |]
translate (Union b Epsilon) = [|  (let (b0:bs) = $(translate b) in (1+b0) : bs) |]
translate (Union Epsilon b) = [|  (let (b0:bs) = $(translate b) in (1+b0) : bs)|]
translate (Union b Atom) = [|  (let (b0:b1:bs) = $(translate b) in (b0) : (b1+1) : bs)|]
translate (Union Atom b) = [|  (let (b0:b1:bs) = $(translate b) in (b0) : (b1+1) : bs)|]
translate (Union a b) = [|  ($(translate a) + $(translate b) )|]
translate (Set (Cycle e)) = [| (1/(1 - $(translate e) )) |]
translate (Set e) = [| exp($(translate e)) |]
translate (Seq e) =  [| (1/(1 - $(translate e) )) |]
translate (Cycle e) = [| (-log(1 - $(translate e) )) |]
translate (Comp g Epsilon) = [|1|]
translate (Comp g Atom) = translate g
translate (Comp g (Prod a b)) = translate (Prod (Comp g a) (Comp g b))
translate (Comp g (Union a b)) = translate (Union (Comp g a) (Comp g b))
translate (Comp g (Set e)) = [| exp($(translate $ Comp g e)) |]
translate (Comp g (Seq e)) = [| (1/(1 - $(translate $ Comp g e) )) |]
translate (Comp g (Cycle e)) = [| (-log(1 - $(translate $ Comp g e) )) |]
translate (Comp g (Comp f h)) = translate $ Comp (Comp g f) h
translate (Var s)= (varE $ mkName s)
translate (Comp g (Var s) )= [| compose (varE $ mkName s) ($(translate g)) |]

class CombSys b where
    gen' :: Int -> (CombExp->b) -> ([(PatQ,ExpQ)],[CombExp])
    gen  :: (CombExp->b) -> Q [Dec]
    gen f  = [d| $(tupP pat) = $(tupE xp) |]
        where 
            xs  = fst $ gen' 0 f
            pat = map fst xs
            xp  = map snd xs

instance CombSys [CombExp] where
    gen' n f = let s = varP (mkName ("x" ++ show n))  in ( [(s,(translate (last $ xs)) ) ], init xs)
        where
            xs = f (Var $ "x" ++ show n)

instance CombSys b => CombSys (CombExp->b) where
    gen' n f = ( (var,(translate (last $ xs)) ) : e, init xs)
        where
            s = "x" ++ show n
            var = varP $ mkName s
            (e,xs)  = gen' (n+1) (f (Var s))
