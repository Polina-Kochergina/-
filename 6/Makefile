comp = gfortran

pattern = *.f95
source = ${wildcard ${pattern}}
obj = ${patsubst %.f95, %.o, ${source}}


main: ${obj}
	${comp} $^ -o $@

%.mod %.o: %.f95
	touch $@
	${comp} -c $<


main.o: mod_fun2_1.mod 



run: main
	./main
result: main input
	./main<input>result
clear:
	rm -f *.o *.out *.mod main
