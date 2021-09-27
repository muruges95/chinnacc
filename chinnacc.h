#define _POSIX_C_SOURCE 200809L // enables certain POSIX extensions (see feature_test_macros)
#include <assert.h>
#include <ctype.h> // contains util functions for testing and mapping chars (such as if they are alpha num)
#include <stdarg.h> // allows one to make use of functions with a variable number of arguments using va_x functons and macros
#include <stdbool.h> // allows one to use true and false in place of 0 and 1
#include <string.h> // contains utility functions for manipulating strings
#include <stdio.h>
#include <stdlib.h>

/** TOKENIZER **/

typedef enum {
	TK_PUNCT,	// Punctuators
	TK_IDENT,	// Identifiers
	TK_NUM,		// Numeric literals
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
};

Token *tokenize(char *p);

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
	ND_BLOCK,		// { ... }
	ND_IF,			// if stmt
	ND_FOR,			// for or while stmt
	ND_DOWHILE,		// do-while
	ND_RETURN,		// return expr;
	ND_EXPR_STMT	// expr;
} NodeKind;

// Local variable
typedef struct Obj Obj;
struct Obj {
	Obj *next; // we store all variables in a linked list for access
	char *name; // var name
	int offset; // offset from rbp (position within stack frame)
};

// AST node type
typedef struct Node Node;

// node data stuct is a mix of a tree and a linked list struct
struct Node {
	NodeKind kind;	// type of node
	Node *next;		// for storing next node in a sequence of statements (block)
	Token *tok;		// token being represented by node (for error msgs)
	
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

	// Linked list of nodes for blocks
	Node *body;
};

typedef struct Function Function;
struct Function {
	Node *body;
	Obj *locals;
	int stack_size;
};

Function *parse(Token *tok);

/** CODEGEN **/
void codegen(Function *prog);

/** UTILITY FNS **/

bool equal(Token *tok, char *fmt);
Token *skip(Token *tok, char *s);

/** ERROR REPORTING **/

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);

