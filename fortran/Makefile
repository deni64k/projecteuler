FC=gfortran
FCFLAGS=-O3 -mtune=native
LDFLAGS=

PROGRAMS=solution

.PHONY: run
run: all
	./solution

all: $(PROGRAMS)

.PRECIOUS: %.f08 %.f03 %.o
%.o: %.f08
	$(FC) $(FCFLAGS) -c $<

%.o: %.f03
	$(FC) $(FCFLAGS) -c $<

%: %.o
	$(FC) $(FCFLAGS) $(LDFLAGS) $^ -o $@

.PHONY: clean veryclean
clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)
