all:
	alex -g Parser/Lex.x
	happy -gca Parser/Par.y
	ghc --make Calc.hs -o ../Calc

clean:
	rm -f *.log *.aux *.hi *.o
	rm -f Parser/*.log Parser/*.aux Parser/*.hi Parser/*.o

