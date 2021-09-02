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
	TK_NUM,		// Numeric literals
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
	ND_ADD,	// +
	ND_SUB,	// -
	ND_MUL,	// *
	ND_DIV,	// /
	ND_NEG,	// unary negate
	ND_EQ,	// ==
	ND_NE,	// !=
	ND_LT,	// <
	ND_LTE,	// <=
	ND_NUM	// Integer
} NodeKind;

// AST node type
typedef struct Node Node;
struct Node {
	NodeKind kind;	// type of node
	Node *lhs;		// left child of node
	Node *rhs;		// right child of node
	int val;		// for num nodes
};

Node *parse(Token *tok);

/** CODEGEN **/
void codegen(Node *node);

/** UTILITY FNS **/

bool equal(Token *tok, char *fmt);
Token *skip(Token *tok, char *s);

/** ERROR REPORTING **/

void error(char *fmt, ...);
void error_at(char *loc, char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);

