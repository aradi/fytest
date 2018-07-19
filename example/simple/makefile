.SUFFIXES:
.SUFFIXES: .f90 .fpp .o .fypp

.PHONY: all
all: test_factorial

include make.arch

%.o: %.f90
	$(FXX) $(FXXOPT) -c -o $@ $<

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) $< $@


TEST_FACTORIAL_OBJS := factorial.o test_factorial.o fytest_lib.o
test_factorial: test_factorial.o
	 $(LN) $(LNOPT) -o $@ $(TEST_FACTORIAL_OBJS)

test_factorial.o: factorial.o fytest_lib.o

.PHONY: clean
clean:
	rm -f *.o *.mod

.PHONY: distclean
distclean: clean
	rm -f test_factorial
