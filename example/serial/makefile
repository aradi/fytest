.SUFFIXES:
.SUFFIXES: .f90 .fpp .o .fypp

TARGETS = test_simple test_fixture test_parameterized test_parameterized2

.PHONY: all
all: $(TARGETS)

vpath % ../src

include make.arch

%.o: %.f90
	$(FXX) $(FXXOPT) -c -o $@ $<

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) $< $@


TEST_MYMATH_OBJS := mymath.o test_mymath.o fytest_lib.o
test_simple: test_simple.o mymath.o fytest_lib.o
	 $(LN) $(LNOPT) -o $@ $^

test_simple.o: mymath.o fytest_lib.o


test_fixture: test_fixture.o mymath.o fytest_lib.o
	$(LN) $(LNOPT) -o $@ $^

test_fixture.o: mymath.o fytest_lib.o


test_parameterized: test_parameterized.o mymath.o fytest_lib.o
	$(LN) $(LNOPT) -o $@ $^

test_parameterized.o: mymath.o fytest_lib.o


test_parameterized2: test_parameterized2.o mymath.o fytest_lib.o
	$(LN) $(LNOPT) -o $@ $^

test_parameterized2.o: mymath.o fytest_lib.o


.PHONY: clean
clean:
	rm -f *.o *.mod

.PHONY: distclean
distclean: clean
	rm -f $(TARGETS)
