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

    inheritance_tree():
        _root(0)
    {}

    void set_root(inheritance_tree_node* root) {
        _root = root;
    }

    inheritance_tree_node* search(const std::string& class_name) {
        if (!_root) {
            return 0;
        }
        return search(_root, class_name);
    }

    err_code insert(const std::string& parent, Class_ class_node) {
        /* classes may not be redefined */
        if (search(class_node->get_name())) {
            return CLASS_REDEFINED;
        }

        /* classes can only inherite from classes that have definitions somewhere */
        /* update: a class can not inherite from `Int', `Bool' nor `String' */
        inheritance_tree_node* parent_node = search(parent);
        if (!parent_node) {
            return PARENT_UNDEFINED;
        } else if (parent_node->_class_node->get_name() == std::string("Int") ||
                parent_node->_class_node->get_name() == std::string("Bool") ||
                parent_node->_class_node->get_name() == std::string("String") ||
                parent_node->_class_node->get_name() == std::string("SELF_TYPE")) {
            return ILLEGAL_PARENT;
        }

        inheritance_tree_node* new_node = new inheritance_tree_node(class_node);
        new_node->_parent = parent_node;

        inheritance_tree_node* sib = parent_node->_child;
        if (!sib) {
            parent_node->_child = new_node;
            return NO_ERROR;
        }
        while (sib->_sibling) {
            sib = sib->_sibling;
        }
        sib->_sibling = new_node;
        return NO_ERROR;
    }

    bool is_subclass(Symbol a, Symbol b);

    inheritance_tree_node* _root;
private:
    inheritance_tree_node* search(inheritance_tree_node* node, const std::string& class_name) {
        inheritance_tree_node* ret = 0;
        if (node->_class_node->get_name() == class_name) {
            return node;
        } else if (node->_sibling && (ret = search(node->_sibling, class_name))) {
            return ret;
        } else if (node->_child && (ret = search(node->_child, class_name))) {
            return ret;
        } else {
            return 0;
        }
    }
};

struct type_env {
    SymbolTable<Symbol, Symbol>* names;
    Class_ curr_class;
    ClassTable* ct;
    inheritance_tree* ig;
};


#endif
