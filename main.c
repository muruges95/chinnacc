#include <assert.h>
#include <ctype.h> // contains util functions for testing and mapping chars (such as if they are alpha num)
#include <stdarg.h> // allows one to make use of functions with a variable number of arguments using va_x functons and macros
#include <stdbool.h> // allows one to use true and false in place of 0 and 1
#include <string.h> // contains utility functions for manipulating strings
#include <stdio.h>
#include <stdlib.h>

// Tokenizer
/* DEFINITIONS */

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

/* ERROR REPORTING */

// Input string
static char *current_input;

// Reports an error and exit prog
// Takes in a var num of error params
static void error(char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	exit(1);
}

static void verror_at(char *loc, char *fmt, va_list ap) {
	int pos = loc - current_input;
	fprintf(stderr, "%s\n", current_input);
	fprintf(stderr, "%*s^ ", pos, ""); // prepends sufficient spaces so that the caret sign is pointing up at the correct char
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	verror_at(loc, fmt, ap);
}

static void error_tok(Token *tok, char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	verror_at(tok->loc, fmt, ap);
}

/* HELPER FUNCTIONS */

// Compares current token string to op
static bool equal(Token *tok, char *op) {
	return memcmp(tok->loc, op, tok->len) == 0
		&& op[tok->len] == '\0'; //check if null terminated string
}

// Asserts that current token matches the char `s`.
// If so, returns the pointer to the next token
static Token *skip(Token *tok, char *s) {
	if (!equal(tok, s)) {
		error_tok(tok, "Expected '%s'", s);
	}
	return tok->next;
}

// Asserts that the current token is TK_NUM
// If so, returns the value of the token
static int get_number(Token *tok) {
	if (tok->kind != TK_NUM) {
		error_tok(tok, "Expected a number");
	}
	return tok-> val;
}

static Token *new_token(TokenKind kind, char *start, char *end) {
	Token *tok = calloc(1, sizeof(Token));
	tok->kind = kind;
	tok->loc = start;
	tok->len = end - start;
	return tok;
}

// Tokenize the char "current_input" and returns the new tokens
static Token *tokenize(void) {
	char *p = current_input;
	Token head = {};
	Token *curr = &head;

    while (*p) {
		// Skip whitespace chars
		if (isspace(*p)) {
			p++;
			continue;
		}

		// Numeric literal
		if (isdigit(*p)) {
			curr = curr->next = new_token(TK_NUM, p, p);
			char *q = p;
			curr->val = strtoul(p, &p, 10); // max we can read in is 1 ul worth of digits, also cant read in hexa or octa digits
			// strtoul also advances pointer p to after the number ends
			curr->len = p - q;
			continue;
		}
		
		// Punctuation
		if (ispunct(*p)){
			curr = curr->next = new_token(TK_PUNCT, p, p+1);
			p++;
			continue;
		}
		error_at(p, "invalid token");
    }

	curr = curr->next = new_token(TK_EOF, p, p);
    return head.next;
}
// Parser

/* DEFINITIONS */

typedef enum {
	ND_ADD,
	ND_SUB,
	ND_MUL,
	ND_DIV,
	ND_NUM // Integer
} NodeKind;

// AST node type
typedef struct Node Node;
struct Node {
	NodeKind kind;	// type of node
	Node *lhs;		// left child of node
	Node *rhs;		// right child of node
	int val;		// for num nodes
};

static Node *new_node(NodeKind kind) {
	Node *node = calloc(1, sizeof(Node));
	node->kind = kind;
	return node;
}

static Node *new_binary_node(NodeKind kind, Node *lhs, Node *rhs) {
	Node *node = new_node(kind);
	node->lhs = lhs;
	node->rhs = rhs;
	return node;
}

static Node *new_numeric_node(int val) {
	Node *node = new_node(ND_NUM);
	node->val = val;
	return node;
}

// TODO: Implement these three functions
// This will be delving into actuating the translation scheme
// Need to write it down before adopting it
// Currently the rest that is being passed down not being used, likely for 
// debugging purposes in future commits
static Node *expr(Token *tok, Token **rest);
static Node *mul(Token *tok, Token **rest);
static Node *primary(Token *tok, Token **rest);

// primary: the units that are unbreakable, start with this,
// will also include the non-terminal involved in the lowest precedence ops
// Translation scheme: primary -> digit | "(" expr ")"
static Node *primary(Token *tok, Token **rest) {
	if (tok->kind == TK_NUM) {
		Node *node = new_numeric_node(tok->val);
		*rest = tok->next;
		return node;
	}

	if (equal(tok, "(")) {
		Node *node = expr(tok->next, &tok);
		*rest = skip(tok, ")");
		return node;
	}

	error_tok(tok, "expected either a number or `(`");
}

// expr: in this case the translation scheme targeting the operators with the 
// lowest precedence, will loop back here from the unbreakable (highest preced
// level) in case there is more to unpack. Its also the entry point non term.
// Translation scheme: expr -> mul | expr "+" mul | expr "-" mul
// With regex (as per chibicc): expr -> mul ("+" mul | "-" mul)* (using this)

static Node *expr(Token *tok, Token **rest) {
	Node *node = mul(tok, &tok);

	for (;;) {
		if (equal(tok, "+")) {
			node = new_binary_node(ND_ADD, node, mul(tok->next, &tok));
			continue;
		}
		if (equal(tok, "-")) {
			node = new_binary_node(ND_SUB, node, mul(tok->next, &tok));
			continue;
		}
		*rest = tok;
		return node; // if reached, exit the loop
	}
}

// mul, the next precedence level. notice how the prev one always has at least
// one non-terminal translation involving the next highest (inc some terminals)
// Translation scheme: mul -> primary | mul "*" primary | mul "/" primary
// With regex (as per chibicc): mul-> primary ("*" primary | "/" primary)*

static Node *mul(Token *tok, Token **rest) {
	Node *node = primary(tok, &tok);

	for (;;) {
		if (equal(tok, "*")) {
			node = new_binary_node(ND_MUL, node, primary(tok->next, &tok));
			continue;
		}
		if (equal(tok, "/")) {
			node = new_binary_node(ND_DIV, node, primary(tok->next, &tok));
			continue;
		}

		*rest = tok;
		return node; // if reached, exit the loop
	}
}

// Codegen

static int depth; // stack depth

static void push(void) {
	printf("  push %%rax\n");
	depth++;
}

static void pop(char *arg) {
	printf("  pop %s\n", arg);
	depth--;
}

static void gen_expr(Node *node) {
	if (node->kind == ND_NUM) {
		printf("  mov $%d, %%rax\n", node->val);
		return;
	}
	// all other node types are binary
	gen_expr(node->rhs); // load rhs val into rax reg
	push(); // push rax reg val into stack
	gen_expr(node->lhs); // load lhs val into rax reg
	pop("%rdi");	// pop rhs val into rdi register
					// swapping with the line above fails as rdi val could be
					// overwritten as we codegen for child nodes

	switch (node->kind) {
		case ND_ADD:
			printf("  add %%rdi, %%rax\n"); // dest is rax
			return;
		case ND_SUB:
			printf("  sub %%rdi, %%rax\n"); // sub source (rdi) from dest (rax)
			return;
		case ND_MUL:
			printf("  imul %%rdi, %%rax\n"); // dest is rax
			return;
		case ND_DIV:
			printf("  cqo\n"); // converts quadword in rax to octoword rdx:rax
			printf("  idiv %%rdi\n"); // signed divide the octoword by rdi
			return;
	}

	error("invalid expression");
}

int main(int argc, char **argv) {
	if (argc != 2){
		error("%s: Expected 2 arguments (filename and input string to be compiled, instead found: %i", argv[0], argc);
	}

	current_input = argv[1];
	Token *tok = tokenize();
	
	// first token must be a number
	// if (tok->kind != TK_NUM) {
		// error_at(current_input, "%s: First non-whitespace token must be a number", argv[0]);
	// }

	Node *node = expr(tok, &tok);

	// if not all tokens consumed when generating parse tree
	if (tok->kind != TK_EOF) {
		error_tok(tok, "extra token not parsed");
	}

	printf("  .globl main\n");
	printf("main:\n");

	// codegen as we walk down parse tree
	gen_expr(node);

	printf("  ret\n");

	assert(depth == 0);
	return 0;
}
