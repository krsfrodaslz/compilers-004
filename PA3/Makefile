CXX=c++

INC= -I.

CFLAGS=-g -Wall -Wno-unused -Wno-deprecated -Wno-write-strings -DDEBUG $(INC)

LIB= -lfl

AR= gar
ARCHIVE_NEW= -cr
RANLIB= gar -qs

CFILES= $(shell ls *.cc) cool-parse.cc

OBJS= $(patsubst %cc, %o, $(filter %.cc, $(CFILES)))

OUTPUT= good.output bad.output

BFLAGS = -d -v -y -b cool --debug -p cool_yy

BISON= bison $(BFLAGS)

DEPEND = $(CXX) -MM $(INC)

.PHONE: all clean

all: parser

parser: $(OBJS)
	$(CXX) $(CFLAGS) -o $@ $^ $(LIB)

.cc.o:
	$(CXX) $(CFLAGS) -c $<

cool-parse.cc: cool.y
	$(BISON) cool.y
	mv -f cool.tab.c cool-parse.cc


$(OUTPUT): parser good.cl bad.cl
	@rm -f $(OUTPUT)
	./myparser good.cl >good.output 2>&1 
	-./myparser bad.cl >bad.output 2>&1 

dotest:	parser good.cl bad.cl
	@echo "\nRunning parser on good.cl\n"
	-./myparser good.cl 
	@echo "\nRunning parser on bad.cl\n"
	-./myparser bad.cl


clean :
	-rm -f $(OUTPUT) *.s $(OBJS)  parser cool-parse.cc *.d cool.output

%.d: %.cc
	$(SHELL) -ec '$(DEPEND) $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include $(CFILES:.cc=.d)
