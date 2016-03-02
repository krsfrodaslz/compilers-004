#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>
#include <string>
#include <utility>  // pair
#include <algorithm>  // find_if, max_element, max

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

typedef std::pair<Feature, Symbol> method_classname_pair;

class method_name_is {
public:
    method_name_is(Symbol name):_name(name)
    {}

    bool operator() (const method_classname_pair& m) {
        return m.first->get_name() == _name;
    }

private:
    Symbol _name;
};

class env_type {
public:
    // type, attr, offset
    typedef std::map<Symbol, std::map<Symbol, int> > attr_offset_container;
    // type, method, formal, offset
    typedef std::map<Symbol, std::map<Symbol, std::map<Symbol, int> > > formal_offset_container;
    // type, method, max_let_and_case_nested_depth
    typedef std::map<Symbol, std::map<Symbol, int> > method_depth_container;
    // type max_let_and_case_nested_depth of attributes
    typedef std::map<Symbol, int> attr_depth_container;
    // local variable (let or case), offset (dynamic)
    typedef std::vector<std::pair<Symbol, int> > local_offset_container;

    enum id_type {
        ATTRIBUTE,
        FORMAL,
        LOCAL,
        SELFOBJ
    };

    env_type():
        classtable(0),
        curr_class(0),
        curr_method(0),
        blc(0),
        curr_depth(0),
        curr_loc(0)
    {}

    // 12, 16, 20, ... (relative to $s0)
    int attr_offset(Symbol attr) {
        assert(curr_class && attr);
        return aoc[curr_class->get_name()][attr];
    }

    // 4, 8, 12, ... (relative to $fp)
    int formal_offset(Symbol formal) {
        assert(curr_class && curr_method && formal);
        return foc[curr_class->get_name()][curr_method->get_name()][formal];
    }

    // -12, -16, -20, ... (relative to $fp)
    int local_offset(Symbol local) {
        assert(curr_class && curr_method && curr_loc && curr_depth && local);
        // iterate reversely to find the first matched one
        for (local_offset_container::const_reverse_iterater rit = curr_loc->rbegin();
                rit != loc->rend(); ++rit) {
            if (rit->first == local) {
                return rit->second;
            }
        }
        assert(0);
    }

    int attr_depth_info() {
        assert(curr_class);
        return adc[curr_class->get_name()];
    }

    int method_depth_info() {
        assert(curr_class && curr_method);
        return mdc[curr_class->get_name()][curr_method->get_name()];
    }

    
    void set_attr_offset(Symbol attr, int n) {
        assert(curr_class && attr);
        aoc[curr_class->get_name()][attr] = n;
    }

    void set_formal_offset(Symbol formal, int n) {
        assert(curr_class && curr_method && formal);
        foc[curr_class->get_name()][curr_method->get_name()][formal] = n;
    }

    void set_local_offset(Symbol local) {
        assert(curr_class && curr_method && curr_loc && curr_depth && local);
        curr_loc->push_back(std::make_pair(local, (FRAME_SIZE+curr_depth-1)*WORD_SIZE));
    }

    void set_attr_depth_info(int depth) {
        assert(curr_class);
        if (depth > adc[curr_class->get_name()]) {
            adc[curr_class->get_name()] = depth;
        }
    }

    void set_method_depth_info(int depth) {
        assert(curr_class && curr_method);
        mdc[curr_class->get_name()][curr_method->get_name()] = depth;
    }

    // lookup an object in the current context.
    int lookup_object(Symbol o, int& offset) {
        assert(curr_class);
        extern Symbol self;
        if (o == self) {
            return SELFOBJ;
        }

        if (curr_method && curr_loc && curr_depth && loc.count(o)) {  // a local (let or case)
            offset = local_offset(o);
            return LOCAL;
        } else if (aoc[curr_class->get_name()].count(o)) {  // an attribute
            offset = attr_offset(o);
            return ATTRIBUTE;
        } else if (curr_method) {  // a formal
            offset = formal_offset(o);
            return FORMAL;
        } else {  // should never reach here
            assert(0);
        }
    }

    CgenClassTableP classtable;
    CgenNodeP curr_class;
    Feature curr_method;

    attr_offset_container aoc;
    formal_offset_container foc;

    method_depth_container mdc;
    attr_depth_container adc;

    int blc; // branch label counter
    int curr_depth;
    local_offset_container* curr_loc;
};

class compare_branch_var_type {
public:
    compare_branch_var_type(CgenClassTableP ct):
        _ct(ct)
    {}

    // FIXME
    bool operator() (const Case& a, const Case& b) {
        CgenNodeP node_a = _ct->probe(a->get_type());
        CgenNodeP node_b = _ct->probe(b->get_type());
        return node_a->get_class_tag() < node_b->get_class_tag();
    }

    CgenClassTableP _ct;
};

void emit_class_names(ostream& s, CgenNodeP node);
void emit_class_object_table(ostream& s, CgenNodeP node);
void emit_dispatch_table(ostream& s, env_type& e, const std::vector<method_classname_pair>& ims);
void emit_prototype_objects(ostream& s, env_type& e, const std::vector<Feature>& ias);
void emit_class_methods(ostream& s, env_type& e);

void emit_precedure_set_up_code(ostream& s);
void emit_precedure_set_up_code(ostream& s, int depth);
void emit_precedure_clean_up_code(ostream& s);
void emit_precedure_clean_up_code(ostream& s, int depth);
void emit_precedure_clean_up_code(ostream& s, int depth, int narg);
