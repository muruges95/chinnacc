#include "chinnacc.h"

int main(int argc, char **argv) {
	if (argc != 2){
		error("%s: Expected 2 arguments (filename and input filename to be compiled, instead found: %i", argv[0], argc);
	}

	Token *tok = tokenize_file(argv[1]);
	Obj *prog = parse(tok);
	codegen(prog);
	return 0;
}
