CXX= c++

INC= -I.

CFLAGS=-g -Wall -Wno-unused -Wno-deprecated -Wno-write-strings $(INC) -DDEBUG

LIB= -lfl

CFILES= $(shell ls *.cc) semant.cc

OBJS= $(patsubst %cc, %o, $(filter %.cc, $(CFILES)))

SEMANT_OBJS := $(filter-out symtab_example.o, $(OBJS))

OUTPUT= good.output bad.output

ASTBFLAGS = -d -v -y -b ast --debug -p ast_yy

DEPEND = $(CXX) -MM $(INC)

.PHONE: all clean

all: semant

semant:  $(SEMANT_OBJS)
	$(CXX) $(CFLAGS) -o $@ $^ $(LIB)

$(OUTPUT): semant
	@rm -f $(OUTPUT)
	./mysemant good.cl >good.output 2>&1 
	-./mysemant bad.cl >bad.output 2>&1 

symtab_example: symtab_example.cc 
	$(CXX) $(CFLAGS) -o $@ $<

.cc.o:
	$(CXX) $(CFLAGS) -c $<

dotest:	semant good.cl bad.cl
	@echo "\nRunning semantic checker on good.cl\n"
	-./mysemant good.cl
	@echo "\nRunning semantic checker on bad.cl\n"
	-./mysemant bad.cl

clean :
	-rm -f $(OUTPUT) *.s $(OBJS) semant symtab_example *.a *.o *.d

%.d: %.cc $(SRC)
	$(SHELL) -ec '$(DEPEND) $< | sed '\''s/\($*\.o\)[ :]*/\1 $@ : /g'\'' > $@'

-include $(CFILES:.cc=.d)
