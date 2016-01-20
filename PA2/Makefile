CXX=c++

INC= -I.

CFLAGS= -g -Wall -Wno-unused -Wno-write-strings $(INC)

LIB= -lfl

CFILES= $(shell ls *.cc) cool-lex.cc

OBJS= $(patsubst %.cc, %.o, $(filter %.cc, $(CFILES)))

DEPEND= $(CXX) -MM $(INC)

FFLAGS= -d

FLEX= flex $(FFLAGS)

.PHONE: all clean

all: lexer

lexer: $(OBJS)
	$(CXX) $(CFLAGS) $^ $(LIB) -o $@

.cc.o:
	$(CXX) $(CFLAGS) -c $<

cool-lex.cc: cool.flex
	$(FLEX) -o $@ $<


test.output: lexer test.cl
	-./lexer test.cl >$@ 2>&1

dotest: lexer test.cl
	./lexer test.cl

clean:
	-rm -f test.output *.s lexer cool-lex.cc *.o *.d

%.d: %.cc cool.flex
	$(SHELL) -ec '$(DEPEND) $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include $(CFILES:.cc=.d)
