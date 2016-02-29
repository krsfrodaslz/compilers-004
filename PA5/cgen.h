#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <string>
#include <utility>  // pair
#include <algorithm>  // find_if

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int class_tag;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   void set_class_tag(int tag) { class_tag = tag; }
   int get_class_tag() { return class_tag; }
   const std::map<Symbol, int>& get_offsets() { return offsets; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


//
//

typedef std::pair<Symbol, Symbol> symbol_pair;

class method_name_is {
public:
    method_name_is(Symbol name):_name(name)
    {}

    bool operator() (const symbol_pair& m) {
        return m.first == _name;
    }

private:
    Symbol _name;
};

class env_type {
public:
    typedef std::map<Symbol, std::map<Symbol, int> > offset_container;
    typedef std::map<Symbol, std::map<Symbol, std::map<Symbol, int> > > formal_order_container;

    env_type():curr_class(0), curr_method(0)
    {}

    // get the offset of attribute `attr' of objects of current class
    int offset(Symbol attr) {
        return offset_container[curr_class->get_name()][attr];
    }

    // get the offset of attribute `attr' of objects of type `type'
    int offset(Symbol type, Symbol attr) {
        return offset_container[type][attr];
    }

    int order(Symbol formal) {
        return formal_order_container[curr_class->get_name()][curr_method->get_name()][formal];
    }

    int order(Symbol method, Symbol formal) {
        return formal_order_container[curr_class->get_name()][method][formal];
    }

    int order(Symbol type, Symbol method, Symbol formal) {
        return formal_order_container[type][method][formal];
    }

    offset_container oc;
    formal_order_container foc;
    CgenNodeP curr_class;
    Feature curr_method;
};

void emit_class_names(ostream& s, CgenNodeP node);
void emit_class_object_table(ostream& s, CgenNodeP node);
// <method_name, class_name>
void emit_dispatch_table(ostream& s, env_type& e, const std::vector<symbol_pair>& ims);
void emit_prototype_objects(ostream& s, env_type& e, const std::vector<Feature>& ias);
void emit_class_methods(ostream& s, env_type& e);

void emit_callee_set_up_code(ostream& s);
void emit_callee_clean_up_code(ostream& s);
void emit_callee_clean_up_code(ostream& s, int narg);
