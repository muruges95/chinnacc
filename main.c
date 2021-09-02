#include "chinnacc.h"

int main(int argc, char **argv) {
	if (argc != 2){
		error("%s: Expected 2 arguments (filename and input string to be compiled, instead found: %i", argv[0], argc);
	}

	Token *tok = tokenize(argv[1]);
	Node *node = parse(tok);
	codegen(node);
	return 0;
}
