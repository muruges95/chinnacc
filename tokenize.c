#include "chinnacc.h"

// Input string
static char *current_input;

// Reports an error and exit prog
// Takes in a var num of error params
void error(char *fmt, ...) {
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

void error_at(char *loc, char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	verror_at(loc, fmt, ap);
}

void error_tok(Token *tok, char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	verror_at(tok->loc, fmt, ap);
}

/* HELPER FUNCTIONS */

// Compares current token string to op
bool equal(Token *tok, char *op) {
	return memcmp(tok->loc, op, tok->len) == 0
		&& op[tok->len] == '\0'; //check if null terminated string
}

// Asserts that current token matches the char `s`.
// If so, returns the pointer to the next token
Token *skip(Token *tok, char *s) {
	if (!equal(tok, s)) {
		error_tok(tok, "Expected '%s'", s);
	}
	return tok->next;
}

static Token *new_token(TokenKind kind, char *start, char *end) {
	Token *tok = calloc(1, sizeof(Token));
	tok->kind = kind;
	tok->loc = start;
	tok->len = end - start;
	return tok;
}

// compares the leading chars with some short string q
static bool startswith(char *p, char *q) {
	return strncmp(p, q, strlen(q)) == 0;
}

// returns the length of the punct to consume
// if one of the equality operators returns 2, otherwise 1 if a punct
static int read_punct(char *p) {
	if (startswith(p, "==") || startswith(p, "!=") ||
				startswith(p, "<=") || startswith(p, ">=")) {
		return 2;
	}

	return ispunct(*p) ? 1 : 0;
}
// Tokenize the input string and returns the new tokens
// current input is a global pointer for error reporting purposes
Token *tokenize(char *p) {
	current_input = p;
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
			// we dont advance the p to the end here as that is being done by strtoul
			curr = curr->next = new_token(TK_NUM, p, p);
			char *q = p;
			curr->val = strtoul(p, &p, 10); // max we can read in is 1 ul worth of digits, also cant read in hexa or octa digits
			// strtoul also advances pointer p to after the number ends
			curr->len = p - q;
			continue;
		}

		// identifier (currently accepts 'a' to 'z' only, lowercase)
		if ('a' <= *p && *p <= 'z') {
			curr = curr->next = new_token(TK_IDENT, p, p+1);
			p++;
			continue;
		}
		
		// Punctuation
		int punct_len = read_punct(p);
		if (punct_len) {
			curr = curr->next = new_token(TK_PUNCT, p, p+punct_len);
			p += punct_len;
			continue;
		}
		error_at(p, "invalid token");
    }

	curr = curr->next = new_token(TK_EOF, p, p);
    return head.next;
}

