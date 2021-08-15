#include <ctype.h> // contains util functions for testing and mapping chars (such as if they are alpha num)
#include <stdarg.h> // allows one to make use of functions with a variable number of arguments using va_x functons and macros
#include <stdbool.h> // allows one to use true and false in place of 0 and 1
#include <string.h> // contains utility functions for manipulating strings
#include <stdio.h>
#include <stdlib.h>

typedef enum {
	TK_PUNCT,	// Punctuators
	TK_NUM,		// Numeric literals
	TK_EOF,		// End of file markers
} TokenKind;

// Token type
typedef struct Token {
	TokenKind kind;	// Token kind
	Token *next;	// next token
	int val;		// if numeric token, its value
	char *loc;		// Token location
	int len;		// Token length
} Token;


int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "%s: invalid number of arguments\n", argv[0]);
        return 1;
    }

    char *p = argv[1];

    printf("  .globl main\n");
    printf("main:\n");
    // printf("  mov $%d, %%rax\n", atoi(argv[1]));

    printf("  mov $%ld, %%rax\n", strtol(p, &p, 10));

    while (*p) {
        if (*p == '+') {
            p++;
            printf("  add $%ld, %%rax\n", strtol(p, &p, 10));
            continue;
        } else if (*p == '-') {
            p++;
            printf("  sub $%ld, %%rax\n", strtol(p, &p, 10));
            continue;
        } else {
            fprintf(stderr, "unexpected character in token stream: '%c'\n", *p);
            return 1;
        }
    }

    printf("  ret\n");
    return 0;
}
