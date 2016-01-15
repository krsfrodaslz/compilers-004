/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

#define CHECK_STRING_LENGTH if (string_too_long()) {\
    BEGIN(broken_str); cool_yylval.error_msg = "String constant too long"; \
    return ERROR; }

int comment_depth = 0;

int string_too_long();
%}

%x comment string broken_str

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
LE              <=
KEYWORD         (?i:class|else|fi|if|in|inherits|isvoid|let|loop|pool|then|while|case|esac|new|of|not)|t(?i:rue)|f(?i:alse)
INT_CONST       [0-9]+
TYPEID          [A-Z][0-9a-zA-Z_]*
OBJECTID        [a-z][0-9a-zA-Z_]*
SPECIAL         [-~@;:.,+*/<={}()]
SLCOMMENT       \-{2}.*
COMMENT         \(\*
WHITESPACE      [ \f\r\t\v]+

%%

 /*
  *  Nested comments
  */
{SLCOMMENT} /* eat up till the end of line */
<comment,INITIAL>{COMMENT}      { ++comment_depth; BEGIN(comment); }
<comment>\*\)                   { if (--comment_depth == 0) { BEGIN(INITIAL); } }
<comment><<EOF>>                { cool_yylval.error_msg = "EOF in comment"; BEGIN(INITIAL); return ERROR; }
<comment>[^*()\n]*
<comment>\*
<comment>\(
<comment>\n                     { ++curr_lineno; }
\*\)                            { cool_yylval.error_msg = "Unmatched *)"; return ERROR; }

 /*
  *  The multiple-character operators.
  */
{DARROW}    { return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
{LE}        { return LE; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
(?i:class)      { return CLASS; }
(?i:else)       { return ELSE; }
(?i:if)         { return IF; }
(?i:fi)         { return FI; }
(?i:in)         { return IN; }
(?i:inherits)   { return INHERITS; }
(?i:isvoid)     { return ISVOID; }
(?i:let)        { return LET; }
(?i:loop)       { return LOOP; }
(?i:pool)       { return POOL; }
(?i:then)       { return THEN; }
(?i:while)      { return WHILE; }
(?i:case)       { return CASE; }
(?i:esac)       { return ESAC; }
(?i:new)        { return NEW; }
(?i:of)         { return OF; }
(?i:not)        { return NOT; }

t(?i:ure)       { cool_yylval.boolean = 1; return BOOL_CONST; }
f(?i:alse)      { cool_yylval.boolean = 0; return BOOL_CONST; }

 /* identifiers */
{TYPEID}        { cool_yylval.symbol = idtable.add_string(yytext); return TYPEID; }
{OBJECTID}      { cool_yylval.symbol = idtable.add_string(yytext); return OBJECTID; }
{INT_CONST}     { cool_yylval.symbol = inttable.add_string(yytext); return INT_CONST; }

 /* special characters (brackets, operators, etc.) */
{SPECIAL}       { return (unsigned char)(yytext[0]); }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"  { string_buf_ptr = string_buf; BEGIN(string); }
<string><<EOF>> { cool_yylval.error_msg = "EOF in string constant"; BEGIN(INITIAL); return ERROR; }
<string>\0      { BEGIN(broken_str); cool_yylval.error_msg = "String contains null character"; return ERROR; }
<string>\\\0    { BEGIN(broken_str); cool_yylval.error_msg = "String contains escaped null character"; return ERROR; }
 /* eat up till string end */
<broken_str>[^"\n]
<broken_str>\"  { BEGIN(INITIAL); }
<broken_str>\n  { ++curr_lineno; BEGIN(INITIAL); }
<string,broken_str>\\\n { ++curr_lineno;  /* escaped newline (multi-line string) */ }
<string>\n      { ++curr_lineno; BEGIN(INITIAL); cool_yylval.error_msg = "Unterminated string constant"; return ERROR; }
<string>\"      { *string_buf_ptr = 0; cool_yylval.symbol = stringtable.add_string(string_buf); BEGIN(INITIAL); return STR_CONST; }
<string>\\n     { CHECK_STRING_LENGTH; *string_buf_ptr++ = '\n'; }
<string>\\f     { CHECK_STRING_LENGTH; *string_buf_ptr++ = '\f'; }
<string>\\b     { CHECK_STRING_LENGTH; *string_buf_ptr++ = '\b'; }
<string>\\t     { CHECK_STRING_LENGTH; *string_buf_ptr++ = '\t'; }
<string>\\.     { CHECK_STRING_LENGTH; *string_buf_ptr++ = yytext[1]; }
<string>.       { CHECK_STRING_LENGTH; *string_buf_ptr++ = yytext[0]; }

\n              { ++curr_lineno; }
{WHITESPACE}    /* ignore whitespaces */

 /* invalid character */
.               { cool_yylval.error_msg = strdup(yytext); return ERROR; }

%%

/* 
 * workaround for new version of flex
 * see the following links for details
 * [http://sourceforge.net/p/flex/bugs/149/]
 * [http://permalink.gmane.org/gmane.linux.lfs.beyond.support/50723]
 * [https://class.coursera.org/compilers-004/forum/thread?thread_id=174#post-818]
 */
#undef yylex
extern "C" int yylex() { return cool_yylex(); }

int string_too_long() {
    if (string_buf_ptr-string_buf >= MAX_STR_CONST-1) {
        return 1;
    }
    return 0;
}
