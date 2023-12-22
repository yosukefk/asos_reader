OBJ = \
main.o \
readone.o \
readline.o \
checkwban.o \
changehr.o \
checkwind.o \
getwinds.o \
leapyr.o \
checkfield.o \
upcase.o \
initial.o \


MOD = mod_main.o

FFLAGS = -O0 -traceback -mieee-fp -fpe1 -save

default: $(MOD) $(OBJ)
	ifort $(FFLGS) $(MOD) $(OBJ) -o asos1reader.x

clean:
	rm -f $(OBJ) $(MOD) *.mod

.SUFFIXES: .f .o .for
	
.f.o:
	ifort $(FFLAGS) -c $<
.for.o:
	ifort $(FFLAGS) -c $<
