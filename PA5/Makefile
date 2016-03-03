CXX= c++

INC= -I.

CFLAGS+=-g -Wall -Wno-unused -Wno-write-strings -Wno-deprecated $(INC) -DDEBUG

LIB= -lfl

CFILES= $(shell ls *.cc) cgen.cc cgen_supp.cc

OBJS= $(patsubst %cc, %o, $(filter %.cc, $(CFILES)))

OUTPUT= good.output bad.output

DEPEND = $(CXX) -MM $(INC)

.PHONE: all clean

cgen: $(OBJS)
	$(CXX) $(CFLAGS) -o $@ $^ $(LIB)
#
# if make turnin says "turnin is up to date.", and you want to turnin
# anyway, you can delete the file 'turnin', and do make turnin again.
#
turnin : $(SRC) $(OUTPUT) turnin-clean
	cd ..; $(CLASSDIR)/bin/turnin $(CLASS) PA$(ASSN)

turnin-clean:
	@rm -f cgen $(OBJS) *.a *.o *~ *.d

$(OUTPUT): cgen
	@rm -f $(OUTPUT)
	./mycoolc  example.cl >example.output 2>&1 

.cc.o:
	$(CXX) $(CFLAGS) -c $<

dotest:	cgen example.cl
	@echo "\nRunning code generator on example.cl\n"
	-./mycoolc example.cl

clean:
	-rm -f $(OUTPUT) cgen *.s $(OBJS) *.a *.o

%.d: %.cc $(SRC)
	$(SHELL) -ec '$(DEPEND) $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include $(CFILES:.cc=.d)
