#include "chinnacc.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

// Start of input string
static char *current_input;

// input filename
static char *current_filename;

// Reports an error and exit prog
// Takes in a var num of error params
void error(char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	exit(1);
}

// Reports an error message in the following format and exit.
// e.g.
// foo.c:10 x = y + 1;
//              ^ <error message here>
static void verror_at(char *loc, char *fmt, va_list ap) {
	// int pos = loc - current_input;
	// fprintf(stderr, "%s\n", current_input);

	// first we need to find the start of the line containing the loc
	char *line = loc;
	// first condition is in case we are on the first line
	while (current_input < line && line[-1] != '\n')
		line--; // keep going backwards till hitting start of input or newline char
	
	char *end = loc;
	while (*end != '\n')
		end++; // go to newline char at end of line. TODO: what happens if error on last line
		// without terminating new line char? -> we make sure there is an ending \n when tokenizing file
	
	// determine the line number
	int line_no = 1;
	for (char *p = current_input; p < line; p++)
		if (*p == '\n')
			line_no++;
	
	// print out line 
	int leading = fprintf(stderr, "%s:%d: ", current_filename, line_no); // returns number of chars written
	fprintf(stderr, "%.*s\n", (int) (end - line), line); // TODO: find out how this format string works

	// error message, taking into account indent needed for the leading part
	int pos = loc - line + leading;

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

// if next set of chars match 'str' return true and consume it in token stream
bool consume(Token **tok_loc, char *str) {
	Token *tok = *tok_loc;
	if (equal(tok, str)) {
		*tok_loc = tok->next;
		return true;
	}
	return false;
}

static Token *new_token(TokenKind kind, char *start, char *end) {
	Token *tok = calloc(1, sizeof(Token));
	tok->kind = kind;
	tok->loc = start;
	tok->len = end - start;
	return tok;
}

// check if char is valid as the leading char of identifier
static bool is_ident_lead(char c) {
	// allowed chars: [a-zA-Z_]
	return (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_');
}

// checks if char is valid as a non-leading char of identifier
static bool is_ident_non_lead(char c) {
	// additionally allows 0-9 as possible chars
	return is_ident_lead(c) || ('0' <= c && c <= '9');
}

// compares the leading chars with some short string q
static bool startswith(char *p, char *q) {
	return strncmp(p, q, strlen(q)) == 0;
}

// assumes it is a valid hex char from 0-9[a-f|A-F]
static int from_hex_char(char c) {
	if ('0' <= c && c <= '9') // is numeric
		return c - '0';
	if ('a' <= c && c <= 'f')
		return c - 'a' + 10;
	return c - 'A' + 10;
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

static bool is_keyword(Token *tok) {
	static char *keyword_list[] = {
		"return", "if", "else", "for", "while", "do", "int", "sizeof", "char"
	};
	// using size of instead of the list size to accomodate growth in list easily
	for (int i=0; i < sizeof(keyword_list) / sizeof(*keyword_list); i++) {
		if (equal(tok, keyword_list[i])) {
			return true;
		}
	}
	return false;
}

// read escape char and return the char it is supposed to represent, meant to be used after reading
// in a leading backslash char
static int read_escaped_char(char **char_pos, char *p) {
	// if octal escape sequence, 1-3 octal numbers
	if ('0' <= *p && *p <= '7') {
		int octal_num = *p++ - '0';
		if ('0' <= *p && *p <= '7') {
			octal_num = (octal_num << 3) + (*p++ - '0');
			if ('0' <= *p && *p <= '7')
				octal_num = (octal_num << 3) + (*p++ - '0');
		}
		*char_pos = p;
		return octal_num;
	}

	// if its a hex sequence
	if (*p == 'x') {
		p++;
		if (!isxdigit(*p))
			error_at(p, "invalid hex escape sequence");
		int hex_num = 0;
		while (isxdigit(*p))
			hex_num = (hex_num << 4) + (from_hex_char(*p++));
		*char_pos = p;
		return hex_num;
	}


	// we will be just skipping a single char for the remaining case(s)
	*char_pos = p + 1;

	// we choose to define the meaning of each escaped char using our original compiler's definition
	// apparently this helps with correctness and security of generated code
	// TODO: read this: https://github.com/rui314/chibicc/wiki/thompson1984.pdf

	switch (*p) {
	case 'a': return '\a';
	case 'b': return '\b';
	case 't': return '\t';
	case 'n': return '\n';
	case 'v': return '\v';
	case 'f': return '\f';
	case 'r': return '\r';
	case 'e': return 27; //  this is a GNU C extension for the ASCII escape char
	default: return *p;
	}
}

static char *find_string_literal_end(char *p) {
	char *start = p - 1;
	// terminate once we reach the closing double quotation char
	for (; *p != '"'; p++) {
		// single line string literals cannot contain these two chars
		if (*p == '\n' || *p == '\0') 
			error_at(start, "unclosed string literal");
		// we skip due to possibility of string literal containing an escaped closing quotation char
		if (*p == '\\')
			p++;
	}
	return p; // return the ponter to the end of the string literal
}

// we will have to manually read in the characters char by char instead of using strndup
// so as to properly handle escaped chars
static Token *read_string_literal(char *start) {
	char *end = find_string_literal_end(start + 1);

	char *buf = calloc(1, end - start); // it might be more than we need but not a big issue (1 extra for each escape char)
	int len = 0; // to determine the true len of the string

	for (char *p = start + 1; p < end;) {
		// start of escaped character
		if (*p == '\\')
			buf[len++] = read_escaped_char(&p, p + 1); // we only increase len by 1
		else
			buf[len++] = *p++;
	}

	Token *tok = new_token(TK_STR, start, end + 1); // factor in closing quotes
	tok->ty = arr_of(ty_char, len + 1); // we need to factor in the null char at the end
	tok->str = buf;
	return tok;
}

// Do one pass to convert tokens containing keywords (classified as identifiers) into keyword type tokens
static void convert_keywords(Token *tok) {
	for (Token *t = tok; t; t = t->next) {
		if (is_keyword(t)) {
			t->kind = TK_KEYWORD;
		}
	}
}

// Tokenize the input string and returns the new tokens
// current input is a global pointer for error reporting purposes
static Token *tokenize(char *filename, char *p) {
	current_input = p;
	current_filename = filename;
	Token head = {};
	Token *curr = &head;

    while (*p) {
		// skip line comments
		if (startswith(p, "//")) {
			p += 2;
			// ignore while no newline encountered
			while (*p != '\n')
				p++;
			continue;
		}

		// skip block comments
		if (startswith(p, "/*")) {
			char *q = strstr(p + 2, "*/"); // finds ptr to first occurence of substr in string
			if (!q)
				error_at(p, "unclosed block comment");
			p = q + 2;
			continue;
		}

		// Skip whitespace chars
		if (isspace(*p)) {
			p++;
			continue;
		}

		// Numeric literal
		if (isdigit(*p)) {
			// we dont advance the p to the end here as that is being done by strtoul
			curr = curr->next = new_token(TK_NUM, p, p);
			char *start = p;
			curr->val = strtoul(p, &p, 10); // max we can read in is 1 ul worth of digits, also cant read in hexa or octa digits
			// strtoul also advances pointer p to after the number ends
			curr->len = p - start;
			continue;
		}

		// string literal
		if (*p == '"') {
			curr = curr->next = read_string_literal(p);
			p += curr->len;
			continue;
		}

		// identifier or possibly keyword
		if (is_ident_lead(*p)) {
			char *start = p;
			do {
				p++; // keep reading and advancing pointer as char is valid as identifier characters
			} while (is_ident_non_lead(*p));
			curr = curr->next = new_token(TK_IDENT, start, p);
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
	convert_keywords(head.next);
    return head.next;
}

// Read contents of file into a char buff. Can currently read up in 4096 characters per batch
static char *read_file(char *path) {
	FILE *fp;

	// read from stdin if filename is "-"
	if (strcmp(path, "-") == 0) {
		fp = stdin;
	} else {
		fp = fopen(path, "r");
		if (!fp)
			error("cannot open %s: %s", path, strerror(errno));
	}

	char *buf;
	size_t buflen;
	FILE *out = open_memstream(&buf, &buflen); // store file in memory

	for (;;) {
		char tmp_buf[4096];
		int n = fread(tmp_buf, 1, sizeof(tmp_buf), fp);
		if (n == 0) // no more bytes left to read
			break;
		fwrite(tmp_buf, 1, n, out);
	}

	if (fp != stdin)
		fclose(fp);
	
	fflush(out); // forces any unwritten bytes from the tmp buf to be written before continuing
	// if its an empty file or does not terminate with a newline
	if (buflen == 0 || buf[buflen - 1] != '\n')
		fputc('\n', out);
	// ensure that after the newline char is trailed by null char.
	// this is because the main tokenization function runs till hitting a null character
	fputc('\0', out);
	fclose(out);
	return buf;
}

Token *tokenize_file(char *path) {
	return tokenize(path, read_file(path));
}