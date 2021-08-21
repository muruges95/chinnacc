#include <ctype.h> // contains util functions for testing and mapping chars (such as if they are alpha num)
#include <stdarg.h> // allows one to make use of functions with a variable number of arguments using va_x functons and macros
#include <stdbool.h> // allows one to use true and false in place of 0 and 1
#include <string.h> // contains utility functions for manipulating strings
#include <stdio.h>
#include <stdlib.h>

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
		
		if (*p == '+' || *p == '-') {
			curr = curr->next = new_token(TK_PUNCT, p, p+1);
			p++;
			continue;
		}
		error_at(p, "invalid token");
    }

	curr = curr->next = new_token(TK_EOF, p, p);
    return head.next;
}

int main(int argc, char **argv) {
	if (argc != 2){
		error("%s: Expected 2 arguments (filename and input string to be compiled, instead found: %i", argv[0], argc);
	}

	current_input = argv[1];
	Token *token = tokenize();
	
	// first token must be a number
	if (token->kind != TK_NUM) {
		error_at(current_input, "%s: First non-whitespace token must be a number", argv[0]);
	}

	printf("  .globl main\n");
	printf("main:\n");
	printf("  mov $%d, %%rax\n", get_number(token));
	token = token->next;

	// sequence must now be op, num, op, num, ...
	// in regex: [`op``num`]*
	while (token->kind != TK_EOF) {
		if (equal(token, "+")) {
			printf("  add $%d, %%rax\n", get_number(token->next));
		} else {
			printf("  sub $%d, %%rax\n", get_number(token->next));
		}
		token = token->next->next;
	}
	printf("  ret\n");
	return 0;
}
