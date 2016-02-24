#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <map>
#include <stack>
#include <set>
#include <algorithm>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
}

void ClassTable::install_basic_classes() {
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    // built-in classes
    Symbol filename = stringtable.add_string("<basic class>");

    Class_ Object_class = class_(Object, No_class,
        append_Features(
            append_Features(
                single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                single_Features(method(type_name, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(idtable.add_string("copy"), nil_Formals(), SELF_TYPE, no_expr()))
        ), filename);

    Class_ IO_class = class_(IO, Object,
        append_Features(
            append_Features(
                append_Features(
                    single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                    single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))
                ),
                single_Features(method(in_string, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))
        ), filename);

    Class_ Int_class = class_(Int, Object,
        single_Features(attr(val, prim_slot, no_expr())), filename);

    Class_ Bool_class = class_(Bool, Object, 
        single_Features(attr(val, prim_slot, no_expr())),filename); 

    Class_ Str_class = class_(Str, Object,
        append_Features(
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(attr(val, Int, no_expr())),
                        single_Features(attr(str_field, prim_slot, no_expr()))
                    ),
                    single_Features(method(length, nil_Formals(), Int, no_expr()))
                ),
                single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))
            ),
            single_Features(method(substr, append_Formals(single_Formals(formal(arg, Int)), 
                single_Formals(formal(arg2, Int))), Str, no_expr()))
        ), filename);

    classes = append_Classes(single_Classes(Str_class), classes);
    classes = append_Classes(single_Classes(Bool_class), classes);
    classes = append_Classes(single_Classes(Int_class), classes);
    classes = append_Classes(single_Classes(IO_class), classes);
    classes = append_Classes(single_Classes(Object_class), classes);


    /* build the corresponding inheritance tree */
    using std::set;
    using std::vector;
    using std::pair;
    using std::map;
    using std::make_pair;
    using std::string;

    set<Symbol> class_set;
    set<Class_> uninserted;
    //map<Symbol, Class_> bt;
    for (int it = classes->first(); classes->more(it); it = classes->next(it)) {
        Class_ cls = classes->nth(it);
        class_set.insert(cls->get_name());
        uninserted.insert(cls);
        //bt.insert(make_pair(cls->get_name(), cls));
    }

    /* class `Main' should be defined */
    if (class_set.find(Main) == class_set.end()) {
        classtable->semant_error() << "class `Main' should be defined\n";
        exit(1);
    }

    vector<pair<inheritance_tree::err_code, Class_> > errors;
    int nerror = 0, nerror_old;

    inheritance_tree ig;
    ig.set_root(new inheritance_tree_node(Object_class));
    uninserted.erase(Object_class);

    /* keep inserting new nodes until we get the final tree */
    do {
        nerror_old = nerror;
        nerror = 0;
        errors.clear();

        for (int it = classes->first(); classes->more(it); it = classes->next(it)) {
            Class_ cls = classes->nth(it);
            if (uninserted.find(cls) == uninserted.end()) {
                continue;
            }
            inheritance_tree::err_code code = ig.insert(cls->get_parent(), cls);
            if (code != inheritance_tree::NO_ERROR) {
                ++nerror;
                errors.push_back(make_pair(code, cls));
            } else {
                uninserted.erase(cls);
            }
        }
    // quit when no error exists or we should abort the program
    } while (nerror != 0 && nerror != nerror_old);   

    /* 
     * If all inheritances are valid (no cycle exists), we'll get one single 
     * inheritance tree with `Object_class' as its root node. Otherwise, 
     * there will be multiple trees. In the latter case, classes in any cycle
     * cannot be inserted into the `Object_class' tree. So if we get only
     * `PARENT_UNDEFINED' errors and some of them are mis-claimed, that means
     * there's at least one cycle in the inheritance graph.
     */
    unsigned int nparent_undefined = 0;
    for (vector<pair<inheritance_tree::err_code, Class_> >::iterator it = errors.begin();
            it != errors.end(); ++it) {
        if (it->first == inheritance_tree::PARENT_UNDEFINED) {
            ++nparent_undefined;
        }
    }
    if (nparent_undefined == errors.size()) {
        for (vector<pair<inheritance_tree::err_code, Class_> >::iterator it = errors.begin();
                it != errors.end(); ++it) {
            /* find a cycle */
            if (class_set.find(it->second->get_parent()) != class_set.end()) {
                classtable->semant_error(it->second) << "inheritance graph is cyclic: class `"
                    << it->second->get_name() << "'\n";
                goto finish;
            }
        }
    }

    /* select one reasonable error to print and exit */
    if (nerror) {
        for (vector<pair<inheritance_tree::err_code, Class_> >::iterator it = errors.begin();
                it != errors.end(); ++it) {
            /* skip mis-claimed `PARENT_UNDEFINED' errors */
            if (it->first == inheritance_tree::PARENT_UNDEFINED &&
                    class_set.find(it->second->get_parent()) != class_set.end()) {
                continue;
            }

            switch (it->first) {
            case inheritance_tree::PARENT_UNDEFINED:
                classtable->semant_error(it->second) << "class `" << it->second->get_parent() << "' undefined\n";
                goto finish;
                break;
            case inheritance_tree::ILLEGAL_PARENT:
                classtable->semant_error(it->second) << "can not inherite from `" << it->second->get_parent() << "'\n";
                goto finish;
                break;
            case inheritance_tree::CLASS_REDEFINED:
                classtable->semant_error(it->second) << "class `" << it->second->get_name() << "' redefined\n";
                goto finish;
            default:
                std::cerr << "unexpected error code: " << it->first << "\n";
                exit(1);
            }
        }
    }

    /* examine the program following the inheritance path (top-down) */

    env_t env;
    env->names = new SymbolTable<Symbol, Symbol>();
    env->ct = classtable;
    env->ig = &ig;
    
    check_type(ig._root, env);

finish:
    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << "\n";
        exit(1);
    }
}

void check_type(inheritance_tree_node* node, env_t env) {
    env->curr_class = node->_class_node;
    
    Class_ cls = node->_class_node;
    Features fts = cls->get_features();

    env->names->enterscope();

    /* perform type checking in current class recursively */
    cls->semant(env);

    /* pass environment to child and siblings */
    if (node->_child) {
        check_type(node->_child, env);
    }

    env->names->exitscope();

    inheritance_tree_node* sib = node->_sibling;
    while (sib) {
        check_type(sib, env);
        sib = sib->_sibling;
    }
}



/* ---------------------------------------- */
/* semantic checking stuff */
/* ---------------------------------------- */

/* 1. no method name may be defined multiple times, same as attribute name
 * 2. method `main' should be defined in class `Main'
 * 3. inherited attributes cannot be redefined
 */
void class__class::semant(env_t env) {

    bool is_main_meth_defined = false;

    std::set<Symbol> method_names, attr_names;

    for (int i = features->first(); features->more(i); i = features->next(i)) {

        Feature ft = features->nth(i);
        if (ft->is_method() && env->names->probe(ft->get_name())) {
            env->ct->semant_error(env->curr_class) << "method redefined: " << ft->get_name() << "\n";
        } else if (!ft->is_method()) {
            if (env->names->lookup(ft->get_name())) {
                env->ct->semant_error(env->curr_class) << "attribute redefined: " << ft->get_name() << "\n";
            }
            Symbol t = ft->get_type();
            if (t == SELF_TYPE) {
                t = name;
            }
            env->names->addid(ft->get_name(), new Symbol(t));
        }

        features->nth(i)->semant(env);

        if (ft->get_name() == main_meth) {
            is_main_meth_defined = true;
        }
    }

    if (name == Main && !is_main_meth_defined) {
        env->ct->semant_error(env->curr_class) << "no method `main' in class `Main'\n";
    }
}

/* 1. the identifiers used in the formal params must be distinct
 * 2. a formal param hides any definition of an attribute of the same name
 * 3. method overriding rule (see the manual for details)
 * 4. formal param cannot have type SELF_TYPE
 * 5. method environment is global to the entire program
 */
void method_class::semant(env_t env) {

    inheritance_tree_node* parent = env->ig->search(env->curr_class->get_parent());
    Feature inherited_version = 0;
    while (parent) {
        inherited_version = parent->_class_node->has_method(name);
        if (inherited_version) {
            break;
        }
    }
    if (inherited_version) {
        // TODO
        if (!this->is_signature_consistent(inherited_version)) {
            env->ct->semant_error(env->curr_class) << "imcompatible signature of overrided method `"
                << name << "'\n";
        }
    }

    env->names->enterscope();

    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal fm = formals->nth(i);
        if (env->names->probe(fm->get_name())) {
            env->ct->semant_error(env->curr_class) << "duplicate formal parameters in method `"
                << name << "'\n";
        }
        if (fm->get_type() == SELF_TYPE) {
            env->ct->semant_error(env->curr_class) << "formal parameter cannot have type `SELF_TYPE'\n";
        } else {
            type_exists(fm->get_type(), env);
        }
        env->names->addid(fm->get_name(), new Symbol(fm->get_type()));
    }

    expr->semant(env);
    Symbol expr_type = expr->get_type();
    Symbol rt = return_type;
    if (rt == SELF_TYPE) {
        rt = env->curr_class->get_name();
    }
    type_exists(rt, env);
    if (!env->ig->is_subclass(expr_type, rt)) {
        env->ct->semant_error(env->curr_class) << "return type " << expr_type
            << " of method `" << name << "' does not confirm to declared return type "
            << rt << "\n";
    }


    env->names->exitscope();
}

/* 1. it's illegal to have attributes named self
 * 2. attributes are visible within their initialization expressions
 */
void attr_class::semant(env_t env) {
    if (name == self) {
        env->ct->semant_error(env->curr_class) << "illegal attribute named self\n";
    }
    
    init->semant(env);
    Symbol init_type = init->get_type();
    Symbol td = type_decl;
    if (td == SELF_TYPE) {
        td = env->curr_class->get_name();
    }
    type_exists(td, env);

    if (init_type != No_type) {
        if (!env->ig->is_subclass(init_type, td)) {
            env->ct->semant_error(env->curr_class) << "type " << init_type
                << " of initialization expression does not confirm to declared type "
                << td << "\n";
        }
    }
}

/* 1. given C the dynamic type of expr, the branch with the least 
 *    type T (C <= T) is selected. otherwise, a runtime error is generated
 * 2. the type of the identifier in each branch must be unique and cannot be SELF_TYPE
 * 3. the static type of the case statement is the LUB of all branches
 */
void typcase_class::semant(env_t env) {
    std::set<Symbol> type_sets;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case c = cases->nth(i);
        if (c->get_type() == SELF_TYPE) {
            env->ct->semant_error(env->curr_class) << "type of identifiers in case statements cannot be `SELF_TYPE'\n";
        }
        if (type_sets.count(c->get_type())) {
            env->ct->semant_error(env->curr_class) << "duplicate branches in case statement\n";
        }
        type_sets.insert(c->get_type());

        c->semant(env);
        type = env->ig->least_common_ancester(type, c->get_return_type());
    }
}

void branch_class::semant(env_t env) {
    env->names->enterscope();
    env->names->addid(name, new Symbol(type_decl));
    expr->semant(env);
    env->names->exitscope();
}

void object_class::semant(env_t env) {
    Symbol* object_type = env->names->lookup(name);
    if (!object_type) {
        env->ct->semant_error(env->curr_class) << "undeclared variable "
            << name << "\n";
        type = Object;
        return;
    }
    type = *object_type;
}

/* 1. the type of the whole assignment expression is the type of `expr'
 */
void assign_class::semant(env_t env) {
    expr->semant(env);
    Symbol expr_type = expr->get_type();
    Symbol object_type = *(env->names->lookup(name));
    if (expr_type == SELF_TYPE) {
        expr_type = env->curr_class->get_name();
    }
    if (!env->ig->is_subclass(expr_type, object_type)) {
        env->ct->semant_error(env->curr_class) << "type " << expr_type
            << " of expression does not confirm to type " << object_type
            << " of variable `" << name << "'\n";
    }
    type = expr_type;
}

void bool_const_class::semant(env_t env) {
    type = Bool;
}

void string_const_class::semant(env_t env) {
    type = Str;
}

void int_const_class::semant(env_t env) {
    type = Int;
}

void new__class::semant(env_t env) {
    if (type_name != SELF_TYPE) {
        type_exists(type_name, env);
    }
    type = type_name;
}

/* 1. type of e_0 must have a method f
 * 2. all arguments and formal parameters must match with each other
 */
void static_dispatch_class::semant(env_t env) {
}

void dispatch_class::semant(env_t env) {
}

void cond_class::semant(env_t env) {
    pred->semant(env);
    if (pred->get_type() != Bool) {
        env->ct->semant_error(env->curr_class) << "predicate of 'if' does not have type `Bool'\n";
    }
    then_exp->semant(env);
    else_exp->semant(env);
    type = env->ig->least_common_ancester(then_exp->get_type(), else_exp->get_type());
}

void block_class::semant(env_t env) {
    Expression e;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        e = body->nth(i);
        e->semant(env);
    }
    type = e->get_type();
}

void let_class::semant(env_t env) {
    Symbol td = type_decl;
    if (type_decl != SELF_TYPE) {
        type_exists(type_decl, env);
        td = env->curr_class->get_name();
    }

    init->semant(env);
    Symbol init_type = init->get_type();
    if (init_type != No_type) {
        if (!env->ig->is_subclass(init_type, td)) {
            env->ct->semant_error(env->curr_class) << "type " << init_type
                << " of initialzation expression does not confirm to type "
                << td << " of variable `" << identifier << "'\n";
        }
    }

    env->names->enterscope();
    env->names->addid(identifier, new Symbol(td));
    body->semant(env);

    env->names->exitscope();
    type = body->get_type();
}

void loop_class::semant(env_t env) {
    pred->semant(env);
    if (pred->get_type() != Bool) {
        env->ct->semant_error(env->curr_class) << "predicate of 'loop' does not have type `Bool'\n";
    }
    body->semant(env);
    type = Object;
}

void isvoid_class::semant(env_t env) {
    e1->semant(env);
    type = Bool;
}

void comp_class::semant(env_t env) {
    e1->semant(env);
    if (e1->get_type() != Bool) {
        env->ct->semant_error(env->curr_class) << "'not' operator is undefined to types other than `Bool'\n";
        type = Object;
        return;
    }
    type = Bool;
}

void neg_class::semant(env_t env) {
    e1->semant(env);
    if (e1->get_type() != Int) {
        env->ct->semant_error(env->curr_class) << "'~' operator is undefined to types other than `Int'\n";
        type = Object;
        return;
    }
    type = Int;
}

#define CHECK_BINARY_OPERATOR(op) \
    e1->semant(env); \
    e2->semant(env); \
    if (e1->get_type() != Int || e2->get_type() != Int) { \
        env->ct->semant_error(env->curr_class) << "'op' operator is undefined to types other than `Int'\n"; \
        type = Object; \
        return; \
    }

void lt_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(<);
    type = Bool;
}

void leq_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(<=);
    type = Bool;
}

void plus_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(+);
    type = Int;
}

void sub_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(-);
    type = Int;
}

void mul_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(*);
    type = Int;
}

void divide_class::semant(env_t env) {
    CHECK_BINARY_OPERATOR(/);
    type = Int;
}

void eq_class::semant(env_t env) {
    e1->semant(env);
    e2->semant(env);
    Symbol e1_type = e1->get_type();
    Symbol e2_type = e2->get_type();
    if ((e1_type == Int || e1_type == Bool || e1_type == Str ||
            e2_type == Int || e2_type == Bool || e2_type == Str) &&
            e1_type != e2_type) {
        env->ct->semant_error(env->curr_class) << "'=' operator is undefined to operands of type "
            << e1_type << " and type " << e2_type << "\n";
        type = Object;
    }
    type = Bool;
}

void no_expr_class::semant(env_t env) {
    type = No_type;
}

/* ---------------------------------------- */
/* extended interface */ 
/* ---------------------------------------- */

Symbol class__class::get_name() {
    return name;
}

Symbol class__class::get_parent() {
    return parent;
}

Features class__class::get_features() {
    return features;
}

Feature class__class::has_method(Symbol ft) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (features->nth(i)->get_name() == ft) {
            return features->nth(i);
        }
    }
    return 0;
}

Symbol method_class::get_name() {
    return name;
}

Symbol method_class::get_type() {
    return return_type;
}

bool method_class::is_method() {
    return true;
}

bool method_class::is_signature_consistent(Feature m) {
    return true;
}

Symbol attr_class::get_name() {
    return name;
}

Symbol attr_class::get_type() {
    return type_decl;
}

bool attr_class::is_method() {
    return false;
}

Symbol formal_class::get_name() {
    return name;
}

Symbol formal_class::get_type() {
    return type_decl;
}

Symbol branch_class::get_type() {
    return type_decl;
}

Symbol branch_class::get_return_type() {
    return expr->get_type();
}


/* ---------------------------------------- */
/* inheritance graph */
/* ---------------------------------------- */
inheritance_tree::inheritance_tree():
    _root(0)
{}

void inheritance_tree::set_root(inheritance_tree_node* root) {
    _root = root;
}

inheritance_tree_node* inheritance_tree::search(Symbol class_name) {
    if (!_root) {
        return 0;
    }
    return search(_root, class_name);
}

inheritance_tree::err_code inheritance_tree::insert(Symbol parent, Class_ class_node) {
    /* classes may not be redefined */
    if (search(class_node->get_name())) {
        return CLASS_REDEFINED;
    }

    /* classes can only inherite from classes that have definitions somewhere */
    /* update: a class can not inherite from `Int', `Bool' nor `String' */
    inheritance_tree_node* parent_node = search(parent);
    if (!parent_node) {
        return PARENT_UNDEFINED;
    } else if (parent_node->_class_node->get_name() == Int ||
            parent_node->_class_node->get_name() == Bool ||
            parent_node->_class_node->get_name() == Str ||
            parent_node->_class_node->get_name() == SELF_TYPE) {
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

inheritance_tree_node* inheritance_tree::search(inheritance_tree_node* node, Symbol class_name) {
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

bool inheritance_tree::is_subclass(Symbol a, Symbol b) {
    inheritance_tree_node* na = search(a);
    if (na) {
        while (na) {
            if (na->_class_node->get_name() == b) {
                return true;
            }
            na = na->_parent;
        }
    }
    return false;
}

Symbol inheritance_tree::least_common_ancester(Symbol a, Symbol b) {
    if (!a) {
        return b;
    }
    if (!b) {
        return a;
    }

    std::set<Symbol> ancesters_a;
    inheritance_tree_node* node = search(a);
    while (node) {
        ancesters_a.insert(node->_class_node->get_name());
        node = node->_parent;
    }
    node = search(b);
    while (node) {
        if (ancesters_a.count(node->_class_node->get_name())) {
            break;
        }
        node = node->_parent;
    }
    return node->_class_node->get_name();
}

bool inheritance_tree::has_type(Symbol t) {
    if (search(t)) {
        return true;
    }
    return false;
}

void type_exists(Symbol t, env_t env) {
    if (!env->ig->has_type(type_name)) {
        env->ct->semant_error(env->curr_class) << "type " << type_name
            << " undefined\n";
    }
}
