main1: main1.o
	gfortran main1.o -o main1
main2: main2.o
	gfortran main2.o -o main2
%.o: %.f95
	gfortran -c $< 
run1: main1
	./main1
run2: main2
	./main2
clear:
	rm -f *.o *.out main1 main2
