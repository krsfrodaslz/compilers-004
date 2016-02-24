#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <vector>
#include <map>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};



/* declaration of inheritance tree structure */

class inheritance_tree_node {
public:
    inheritance_tree_node(Class_ class_node):
        _class_node(class_node),
        _parent(0),
        _sibling(0),
        _child(0)
    {}

    Class_ _class_node;

    inheritance_tree_node *_parent, *_sibling, *_child;
};

class inheritance_tree {
public:
    enum err_code {
        NO_ERROR,
        PARENT_UNDEFINED,
        ILLEGAL_PARENT,
        CLASS_REDEFINED,
    };

    inheritance_tree();

    void set_root(inheritance_tree_node* root);

    inheritance_tree_node* search(Symbol class_name);

    err_code insert(Symbol parent, Class_ class_node);

    bool is_subclass(Symbol a, Symbol b);

    Symbol least_common_ancester(Symbol a, Symbol b);

    bool has_type(Symbol t);

    inheritance_tree_node* _root;
private:
    inheritance_tree_node* search(inheritance_tree_node* node, Symbol class_name);
};

struct type_env {
    type_env():names(0), curr_class(0), ct(0), ig(0) {
    }

    SymbolTable<Symbol, Symbol>* names;
    Class_ curr_class;
    ClassTable* ct;
    inheritance_tree* ig;
};

void check_type(inheritance_tree_node* node, env_t env);

void type_exists(Symbol t, env_t env);

#endif
