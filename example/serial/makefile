.SUFFIXES:
.SUFFIXES: .f90 .fpp .o .fypp

TARGETS = test_simple test_full

.PHONY: all
all: $(TARGETS)

vpath % ../src

include make.arch

%.o: %.f90
	$(FXX) $(FXXOPT) -c -o $@ $<

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) $< $@


TEST_FACTORIAL_OBJS := factorial.o test_factorial.o fytest_lib.o
test_simple: test_simple.o factorial.o fytest_lib.o
	 $(LN) $(LNOPT) -o $@ $^

test_simple.o: factorial.o fytest_lib.o

test_full: test_full.o factorial.o fytest_lib.o
	$(LN) $(LNOPT) -o $@ $^

test_full.o: factorial.o fytest_lib.o


.PHONY: clean
clean:
	rm -f *.o *.mod

.PHONY: distclean
distclean: clean
	rm -f $(TARGETS)
