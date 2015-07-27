all: Test

Test: 
	ghc Test_GFOpt.hs -o Test -O2

clean:
	rm *.hi *.o Test *.dyn_o *.dyn_hi *~
