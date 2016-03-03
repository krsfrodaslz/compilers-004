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
   bool is_ancestor(Symbol t1, Symbol t2);
   void set_class_tags(CgenNodeP node, int& count);
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

// The following two classes are auxiliary classes for
// using std::find_if because we need extra parameters
// when finding a matching element.
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

class has_local {
public:
    has_local(Symbol local):_local(local)
    {}

    bool operator() (const std::pair<Symbol, int>& p) {
        return p.first == _local;
    }
private:
    Symbol _local;
};


class env_type {
public:
    // type, attr, offset
    typedef std::map<Symbol, std::map<Symbol, int> > attr_offset_container;
    // type, method_name, offset
    typedef std::map<Symbol, std::map<Symbol, int> > method_offset_container;
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
        SELFOBJ,
        INVALID
    };

    env_type():
        classtable(0),
        curr_class(0),
        curr_method(0),
        blc(0),
        curr_depth(0),
        curr_loc(0)
    {}

    // 3, 4, 5, ... (relative to $s0, in WORDs)
    int attr_offset(Symbol attr) {
        assert(curr_class && attr);
        return aoc[curr_class->get_name()][attr];
    }

    // 1, 2, 3, ... (relative to $fp, in WORDs)
    int formal_offset(Symbol formal) {
        assert(curr_class && curr_method && formal);
        return foc[curr_class->get_name()][curr_method->get_name()][formal];
    }

    // -3, -4, -5, ... (relative to $fp, in WORDs)
    int local_offset(Symbol local) {
        assert(curr_class && curr_method && curr_loc && curr_depth && local);
        // iterate reversely to find the first matched one
        // fix: we should start from begin() + curr_depth - 1
        for (local_offset_container::const_reverse_iterator rit = curr_loc->rbegin();
                rit != curr_loc->rend(); ++rit) {
            if (rit->first == local) {
                return rit->second;
            }
        }
        assert(0);
        return -1;  // makes compiler happy
    }

    // 0, 4, 8, ... (relative to the label of dispatch table)
    int method_offset(Symbol type, Symbol method) {
        assert(type && method);
#ifdef DEBUG
        if (moc.count(type) && moc[type].count(method)) {
            return moc[type][method];
        }
        assert(0);
        return -1;
#else
        return moc[type][method];
#endif
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

    void set_method_offset(Symbol method, int n) {
        assert(curr_class && method);
        moc[curr_class->get_name()][method] = n;
    }

    void set_local_offset(Symbol local) {
        assert(curr_class && curr_method && curr_loc && curr_depth && local);
        curr_loc->push_back(std::make_pair(local, FRAME_SIZE+curr_depth-1));
    }

    void pop_the_last_local() {
        assert(curr_loc->size() > 0);
        curr_loc->pop_back();
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
    id_type lookup_object(Symbol o, int& offset) {
        assert(curr_class);
        extern Symbol self;
        if (o == self) {
            return SELFOBJ;
        }

        if (curr_method && curr_loc && curr_depth &&
                std::find_if(curr_loc->begin(), curr_loc->end(), has_local(o))
                != curr_loc->end()) {  // a local (let or case)
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
            return INVALID;
        }
    }

    CgenClassTableP classtable;
    CgenNodeP curr_class;
    Feature curr_method;

    attr_offset_container aoc;
    formal_offset_container foc;
    method_offset_container moc;

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

    bool operator() (const Case& a, const Case& b) {
        CgenNodeP node_a = _ct->probe(a->get_type());
        CgenNodeP node_b = _ct->probe(b->get_type());
        return node_a->get_class_tag() < node_b->get_class_tag();
    }

    CgenClassTableP _ct;
};

