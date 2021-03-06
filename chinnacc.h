#define _POSIX_C_SOURCE 200809L // enables certain POSIX extensions (see feature_test_macros)
#include <assert.h>
#include <ctype.h> // contains util functions for testing and mapping chars (such as if they are alpha num)
#include <stdarg.h> // allows one to make use of functions with a variable number of arguments using va_x functons and macros
#include <stdbool.h> // allows one to use true and false in place of 0 and 1
#include <string.h> // contains utility functions for manipulating strings
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

typedef struct Type Type;

/** TOKENIZER **/

typedef enum {
	TK_PUNCT,	// Punctuators
	TK_IDENT,	// Identifiers
	TK_NUM,		// Numeric literals
	TK_STR,		// String literals
	TK_KEYWORD,	// Keywords
	TK_EOF,		// End of file markers
} TokenKind;

// Token type
typedef struct Token Token;
struct Token {
	TokenKind kind;	// Token kind
	Token *next;	// next token
	int val;		// if numeric token, its value
	char *loc;		// Token location
	int len;		// Token length

	// strings
	Type *ty;		// used only for strings
	char *str;		// string value
};

Token *tokenize_file(char *filename);

/** PARSE **/
typedef enum {
	ND_NUM,			// Integer
	ND_VAR,			// Variable
	ND_NEG,			// -expr
	ND_ADDR,		// &expr
	ND_DEREF,		// *expr
	ND_ADD,			// expr1 + expr2
	ND_SUB,			// expr1 - expr2
	ND_MUL,			// expr1 * expr2
	ND_DIV,			// expr1 / expr2
	ND_ASSIGN,		// expr1 = expr2
	ND_EQ,			// expr == expr2
	ND_NE,			// expr1 != expr2
	ND_LT,			// expr1 < expr2
	ND_LTE,			// expr1 <= expr2
	ND_FNCALL,		// Function call
	ND_BLOCK,		// { ... }
	ND_IF,			// if stmt
	ND_FOR,			// for or while stmt
	ND_DOWHILE,		// do-while
	ND_RETURN,		// return expr;
	ND_EXPR_STMT,	// expr;
	ND_STMT_EXPR	// (block)  , used where an expr is expected
} NodeKind;

// Variables and Functions
typedef struct Obj Obj;

// AST node type
typedef struct Node Node;

struct Obj {
	Obj *next;		// we store all variables in a linked list for access
	char *name;		// var name
	Type *ty;		// Type
	bool is_local;	// Is local variable (vs global var/function)

	// Only for local variables
	int offset;		// offset from rbp (position within stack frame)

	bool is_function; // to distinguish btween global var and fn

	// Only for global variables
	char *init_data;

	// Only for functions
	Obj *params;
	Node *body;
	Obj *locals;
	int stack_size;
};

// node data stuct is a mix of a tree and a linked list struct
struct Node {
	NodeKind kind;	// kind of node
	Node *next;		// for storing next node in a sequence of statements (block)
	Token *tok;		// token being represented by node (for error msgs)
	Type *ty;		// type of node (e.g. int vs int ptr)
	Node *lhs;		// left child of node
	Node *rhs;		// right child of node
	
	int val;		// used if kind == ND_NUM
	Obj *var;		// used if kind == ND_VAR

	// conditional based expr/stmts
	Node *cond;

	// if stmts
	Node *then; // also used as the stmt body in some stmts
	Node *els;

	// for stmts
	Node *init;
	Node *inc;

	// Linked list of nodes for blocks and stmt expressions
	Node *body;

	// For Function calls
	char *fnname;
	Node *args;
};

Obj *parse(Token *tok);

// Node type fns

typedef enum {
	TY_CHAR,
	TY_INT,
	TY_PTR,
	TY_FN,
	TY_ARR,
} TypeKind;

struct Type {
	TypeKind kind;

	int size; 	// sizeof value

	// represents either pointer to or array of, as they are treated similarly
	// this is why we can treat ptrs and arrays similarly by checking on this base var instead of the 'kind' field
	Type *base;

	// Function return type
	Type *return_ty;
	Type *params;
	Type *next;

	// for arrays only
	int array_len;

	// storing the token where we do the type decl
	Token *name;
};

// default type objects for elementary types, so we dont need to keep recreating it
// think of a singleton in OOP?
extern Type *ty_char;
extern Type *ty_int;

bool is_integer(Type *ty);
Type *pointer_to(Type *base);
Type *copy_type(Type *ty);
Type *fn_type(Type *return_ty);
Type *arr_of(Type *base, int size);
void add_type(Node *node);

/** CODEGEN **/
void codegen(Obj *prog, FILE *out);

/** UTILITY FNS **/

bool equal(Token *tok, char *s);
Token *skip(Token *tok, char *s);
bool consume(Token **tok_loc, char *str);

// In strings file

char *format(char *fmt, ...);

/** ERROR REPORTING **/

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
