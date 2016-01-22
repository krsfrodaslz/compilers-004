/*
*  cool.y
*              Parser definition for the COOL language.
*
*/
%{
  #include <iostream>
  #include "cool-tree.h"
  #include "stringtab.h"
  #include "utilities.h"
  
  extern char *curr_filename;
  
  
  /* Locations */
  #define YYLTYPE int              /* the type of locations */
  #define cool_yylloc curr_lineno  /* use the curr_lineno from the lexer
                                      for the location of tokens */
    
  extern int node_lineno;          /* set before constructing a tree node
                                      to whatever you want the line number
                                      for the tree node to be */
    
    
  #define YYLLOC_DEFAULT(Current, Rhs, N)         \
        Current = Rhs[1];                         \
        node_lineno = Current;
  
  
  #define SET_NODELOC(Current)  \
        node_lineno = Current;
  
  /* IMPORTANT NOTE ON LINE NUMBERS
  *********************************
  * The above definitions and macros cause every terminal in your grammar to 
  * have the line number supplied by the lexer. The only task you have to
  * implement for line numbers to work correctly, is to use SET_NODELOC()
  * before constructing any constructs from non-terminals in your grammar.
  * Example: Consider you are matching on the following very restrictive 
  * (fictional) construct that matches a plus between two integer constants. 
  * (SUCH A RULE SHOULD NOT BE  PART OF YOUR PARSER):
  
  plus_consts	: INT_CONST '+' INT_CONST 
  
  * where INT_CONST is a terminal for an integer constant. Now, a correct
  * action for this rule that attaches the correct line number to plus_const
  * would look like the following:
  
  plus_consts	: INT_CONST '+' INT_CONST 
  {
    // Set the line number of the current non-terminal:
    // ***********************************************
    // You can access the line numbers of the i'th item with @i, just
    // like you acess the value of the i'th exporession with $i.
    //
    // Here, we choose the line number of the last INT_CONST (@3) as the
    // line number of the resulting expression (@$). You are free to pick
    // any reasonable line as the line number of non-terminals. If you 
    // omit the statement @$=..., bison has default rules for deciding which 
    // line number to use. Check the manual for details if you are interested.
    @$ = @3;
    
    
    // Observe that we call SET_NODELOC(@3); this will set the global variable
    // node_lineno to @3. Since the constructor call "plus" uses the value of 
    // this global, the plus node will now have the correct line number.
    SET_NODELOC(@3);
    
    // construct the result node:
    $$ = plus(int_const($1), int_const($3));
  }
  
  */
  
  void yyerror(const char *s);        /*  defined below; called for each parse error */
  extern int yylex();           /*  the entry point to the lexer  */
  
  /************************************************************************/
  /*                DONT CHANGE ANYTHING IN THIS SECTION                  */
  
  Program ast_root;	            /* the result of the parse  */
  Classes parse_results;        /* for use in semantic analysis */
  int omerrs = 0;               /* number of errors in lexing and parsing */
%}
  
/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
Declare the terminals; a few have types for associated lexemes.
The token ERROR is never used in the parser; thus, it is a parse
error when the lexer returns it.

The integer following token declaration is the numeric constant used
to represent that token internally.  Typically, Bison generates these
on its own, but we give explicit numbers to prevent version parity
problems (bison 1.25 and earlier start at 258, later versions -- at
257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/

/* Complete the nonterminal list below, giving a type for the semantic
value of each non terminal. (See section 3.6 in the bison 
documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <feature> feature         /* methods and attrs */
%type <features> feature_list
%type <formal> formal           /* formal params in method definitions */
%type <formals> formal_list
%type <case_> case              /* branch */
%type <cases> case_list
%type <expression> expr let_expr    /* assign, dispatch, cond, etc. */
%type <expressions> expr_list expr_block

%start program

/* Precedence declarations go here. */
%right ASSIGN 
%left NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID  
%left '~'
%left '@'
%left '.'

//%error-verbose

%%
/* 
Save the root of the abstract syntax tree in a global variable.
*/
program:
  class_list    { 
    @$ = @1;
    SET_NODELOC(@1);
    ast_root = program($1); }
;

class_list: 
  class             { /* single class (at least one class) */
    $$ = single_Classes($1);
    parse_results = $$; }
| class_list class  {
    $$ = append_Classes($1,single_Classes($2));
    parse_results = $$; }
  /* If there's an error in a class definition but the class is terminated
     properly and the next class is syntactically correct, the parser should
     be able to restart at the next class definition. */
| error ';' {
    yyclearin; $$ = NULL; }
;

/* If no parent is specified, the class inherits from the Object class. */
class:
  CLASS TYPEID '{' feature_list '}' ';'     {
    @$ = @6;
    SET_NODELOC(@6);
    $$ = class_($2,idtable.add_string("Object"),$4, stringtable.add_string(curr_filename)); }
| CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'     { 
    @$ = @8;
    SET_NODELOC(@8);
    $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
;

/* Feature list may be empty, but no empty features in list. */
feature_list:
  %empty    { /* no features */
    $$ = nil_Features(); }
| feature   {
    $$ = single_Features($1); }
| feature_list feature      {
    $$ = append_Features($1, single_Features($2)); }
  /* The parser should recover from errors in features. */
| error ';' {
    yyclearin; $$ = NULL; }
;

feature:
  OBJECTID ':' TYPEID ';'       { /* one attr (default initialization) */
    /* TODO It's illegal to have attributes named self. */
    @$ = @4;
    SET_NODELOC(@4);
    $$ = attr($1, $3, no_expr()); }
| OBJECTID ':' TYPEID ASSIGN expr ';'   { /* attr with initialization */
    @$ = @6;
    SET_NODELOC(@6);
    $$ = attr($1, $3, $5); }
| OBJECTID '(' formal_list ')' ':' TYPEID '{' expr '}' ';'     { /* a method */
    @$ = @10;
    SET_NODELOC(@10);
    $$ = method($1, $3, $6, $8); }
;

formal_list:
  %empty    { /* no formal parameters */
    $$ = nil_Formals(); }
| formal    {
    $$ = single_Formals($1); }
| formal_list ',' formal    {
    $$ = append_Formals($1, single_Formals($3)); }
;

formal:
  OBJECTID ':' TYPEID   {
    /* TODO It's illegal to have formal parameters named 'self'. */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = formal($1, $3); }
;


/* Case statement */
case_list:
  case      {   /* at least one case statement */
    $$ = single_Cases($1); }
| case_list case    {
    $$ = append_Cases($1, single_Cases($2)); }
;

case:
  OBJECTID ':' TYPEID DARROW expr ';'   {
    /* TODO It's illegal to bind 'self' in 'case'. */
    @$ = @6;
    SET_NODELOC(@6);
    $$ = branch($1, $3, $5); }
;


/* expressions */
expr_list:
  %empty    {
    $$ = nil_Expressions(); }
| expr      {
    $$ = single_Expressions($1); }
| expr_list ',' expr    {
    $$ = append_Expressions($1, single_Expressions($3)); }
;

/* let statement */
let_expr:
  OBJECTID ':' TYPEID IN expr   {
    @$ = @5;
    SET_NODELOC(@5);
    $$ = let($1, $3, no_expr(), $5); }
| OBJECTID ':' TYPEID ASSIGN expr IN expr   {
    @$ = @7;
    SET_NODELOC(@7);
    $$ = let($1, $3, $5, $7); }
| OBJECTID ':' TYPEID ',' let_expr  {
    @$ = @5;
    SET_NODELOC(@5);
    $$ = let($1, $3, no_expr(), $5); }
| OBJECTID ':' TYPEID ASSIGN expr ',' let_expr  {
    @$ = @7;
    SET_NODELOC(@7);
    $$ = let($1, $3, $5, $7); }
  /* The parser should recover from errors in a 'let' binding (going
     on to the next variable). */
| error IN expr     {
    yyclearin; $$ = NULL; }
| error ',' let_expr    {
    yyclearin; $$ = NULL; }
;

expr_block:
  expr ';'  {
    $$ = single_Expressions($1); }
| expr_block expr ';'   {
    $$ = append_Expressions($1, single_Expressions($2)); }
  /* The parser should recover from errors inside an expr block. */
| error ';'  {
    yyclearin; $$ = NULL; }
;

expr:
  OBJECTID      { /* objects */
    @$ = @1;
    SET_NODELOC(@1);
    $$ = object($1); }
| INT_CONST     { /* integers */
    @$ = @1;
    SET_NODELOC(@1);
    $$ = int_const($1); }
| STR_CONST     { /* string constants */
    @$ = @1;
    SET_NODELOC(@1);
    $$ = string_const($1); }
| BOOL_CONST    { /* booleans */
    @$ = @1;
    SET_NODELOC(@1);
    $$ = bool_const($1); }
| OBJECTID ASSIGN expr    { /* assignment */
    @$ = @3;
    SET_NODELOC(@3);
    /* TODO It's illegal to assign to 'self'. */
    $$ = assign($1, $3); }
| expr '@' TYPEID '.' OBJECTID '(' expr_list ')'    { /* static dispatch */
    @$ = @8;
    SET_NODELOC(@8);
    $$ = static_dispatch($1, $3, $5, $7); }
| expr '.' OBJECTID '(' expr_list ')'   { /* dispatch */
    @$ = @6;
    SET_NODELOC(@6);
    $$ = dispatch($1, $3, $5); }
| OBJECTID '(' expr_list ')'  { /* dispatch (shorthand) */
    @$ = @4;
    SET_NODELOC(@4);
    $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
| IF expr THEN expr ELSE expr FI    { /* cond */ 
    @$ = @7;
    SET_NODELOC(@7);
    $$ = cond($2, $4, $6); }
| WHILE expr LOOP expr POOL     { /* loop */
    @$ = @5;
    SET_NODELOC(@5);
    $$ = loop($2, $4); }
| CASE expr OF case_list ESAC   { /* typcase */ 
    @$ = @5;
    SET_NODELOC(@5);
    $$ = typcase($2, $4); }
| '{' expr_block '}'     { /* block */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = block($2); }
| NEW TYPEID    { /* new */
    @$ = @2;
    SET_NODELOC(@2);
    $$ = new_($2); }
| ISVOID expr   { /* isvoid */
    @$ = @2;
    SET_NODELOC(@2);
    $$ = isvoid($2); }
| expr '+' expr     { /* sumation */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = plus($1 ,$3); }
| expr '-' expr     { /* subtraction */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = sub($1 ,$3); }
| expr '*' expr     { /* multiplication */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = mul($1 ,$3); }
| expr '/' expr     { /* division */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = divide($1 ,$3); }
| '~' expr      { /* negation */
    @$ = @2;
    SET_NODELOC(@2);
    $$ = neg($2); }
| expr '<' expr     { /* less than */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = lt($1 ,$3); }
| expr LE expr     { /* less equal than */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = leq($1 ,$3); }
| expr '=' expr     { /* equal */
    @$ = @3;
    SET_NODELOC(@3);
    $$ = eq($1 ,$3); }
| NOT expr      { /* compensation */
    @$ = @2;
    SET_NODELOC(@2);
    $$ = comp($2); }
| '(' expr ')'  {
    @$ = @3;
    SET_NODELOC(@3);
    $$ = $2; }
| LET let_expr  { /* let */
    /* TODO It's illegal to bind 'self' in 'case'. */
    /* When parsing a 'let' expr with multiple identifiers,
       it should be transformed into nested 'let's. */
    @$ = @2;
    SET_NODELOC(@2);
    $$ = $2; }
;

/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  extern int curr_lineno;
  
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
  << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;
  
  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}
