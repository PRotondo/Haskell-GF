# Haskell-GF
Haskell modules to compute with combinatorial specifications involving the
primitives Set, Seq, Cycle, Union and Prod. Results always correspond to
labelled structures.

EXAMPLE

To compute with the system
                { S = Set(C), C = Cycle(T), T = Prod(Atom,Set(T))  },
which correspond to functional graphs (sets of cycles of rooted trees!),
we write
 gen (\(x0 :: C) (x1 :: C) (x2:: C) -> [ Set x1, Cycle x2, Prod Atom (Set x2)]),
and then the list x0 :: [Rational] gives us the coefficients of the PowerSeries
for the S defined above. In other words, (x0!!n) multiplied by factorial(n)
gives you the number of functional graphs with n nodes (which should equal n^n).

The file Test_GFOpt.hs gives
./Test 
(x0, x1, x2) = (GHC.Float.exp x1,
                GHC.Num.negate (GHC.Float.log (1 GHC.Num.- x2)),
                0 GHC.Types.: GHC.Float.exp x2)
4503599627370496000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 % 221029011683193239749619918192033883444416078981211101498573662096150208756912276603371097425227081600675465895410695527056246966969525329318852030773688151050208205069621911299774017406559032095642799762224419422538926234949832869833911139810093390892205153602739123319915066062737
which is exactly to 200^200 / 200!.
