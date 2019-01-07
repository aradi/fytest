.SUFFIXES:
.SUFFIXES: .f90 .fpp .o .fypp

TARGETS = test_simple

.PHONY: all
all: $(TARGETS)

vpath % ../src

include make.arch

%.o: %.f90
	$(FXX) $(FXXOPT) -c -o $@ $<

%.f90: %.fpp
	$(FYPP) $(FYPPOPT) $< $@


test_simple: test_simple.o fytest_lib.o
	 $(LN) $(LNOPT) -o $@ $^

test_simple.o: fytest_lib.o


.PHONY: clean
clean:
	rm -f *.o *.mod

.PHONY: distclean
distclean: clean
	rm -f $(TARGETS)
