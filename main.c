#include "chinnacc.h"
#include <stdio.h>
#include <string.h>

static char *opt_o; // output filename option

static char *input_path;

// print usage message and exit with status
static void usage(int status) {
	fprintf(stderr, "chinnacc [ -o <path> ] <filename>\n");
	exit(status);
}

// parse arguments and filename
static void parse_args(int argc, char **argv) {
	for (int i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "--help"))
			usage(0); // similar to gcc --help
		
		// specifying output filename
		if (!strcmp(argv[i], "-o")) {
			if (!argv[++i]) // output filename not specified as next immediate arg
				usage(0);
			opt_o = argv[i];
			continue; // move on to next option if any
		}
		// in case there is no space between output option flag and value
		if (!strncmp(argv[i], "-o", 2)) {
			opt_o = argv[i] + 2; // skip the "-o" part
			continue;
		}
		// some other argument we have not handled above
		if (argv[i][0] == '-' && argv[i][1] != '\0')
			error("unknown argument: %s", argv[i]);

		input_path = argv[i];
	}

	if (!input_path)
		error("no input file");
}

static FILE *open_output_file(char *path) {
	// if "-" specified as the output name, write to stdout as well
	if (!path || strcmp(path, "-") == 0)
		return stdout;
	FILE *out = fopen(path, "w");
	if (!out)
		error("cannot open output file %s: %s", path, strerror(errno));
	return out;
}

int main(int argc, char **argv) {
	parse_args(argc, argv);

	Token *tok = tokenize_file(input_path);
	Obj *prog = parse(tok);
	FILE *out = open_output_file(opt_o);
	codegen(prog, out);
	return 0;
}
