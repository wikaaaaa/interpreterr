interpreter:
	/home/students/inf/PUBLIC/MRJP/bin/bnfc -m --functor -o generated gramar.cf 
	make -C generated
	ghc -i.:generated RunInterpreter.hs -o interpreter
clean:
	rm -f *.o *.hi interpreter TypeChecker/*.o TypeChecker/*.hi Interpreter/*.o Interpreter/*.hi
	make -C generated distclean
	