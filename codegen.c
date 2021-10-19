#include "chinnacc.h"
#include <stdarg.h>
#include <stdio.h>

static FILE *output_file;
static int depth = 0; // stack depth
// conventional registers used for first 6 args of function in a fn call, in the correct order
static char *argregs8[] = { "%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b" };
static char *argregs64[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
static Obj *current_fn; // stacktracee of fns to know where to jmp to

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

// similar to the java function, appends a newline to the format string
// we also make use of this util function to write to the correct output stream
static void println(char *fmt_string, ...) {
	va_list ap;
	va_start(ap, fmt_string);
	vfprintf(output_file, fmt_string, ap);
	fprintf(output_file, "\n");
	va_end(ap);
}

// for generating sections to jump to
static int count(void) {
	static int i = 1;
	return i++;
}

static void push(void) {
	println("  push %%rax");
	depth++;
}

static void pop(char *arg) {
	println("  pop %s", arg);
	depth--;
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
static int align_to(int n, int align) {
	return ((align + n - 1) / align) * align;
}

// compute the absolute address of a given node and loads it into %rax
// Gives an error if a given node does not reside in memory (i.e. not an lval)
static void gen_addr_and_load(Node *node) {
	switch (node->kind) {
	case ND_VAR:
		if (node->var->is_local) {
			// local var
			println("  lea %d(%%rbp), %%rax", node->var->offset);
		} else {
			// global var
			println("  lea %s(%%rip), %%rax", node->var->name);
		}
		return;
	case ND_DEREF:
		gen_expr(node->lhs);
		return;
	default:;
	}

	error_tok(node->tok, "invalid expression");
}

// Load a value from where %rax is pointing to
static void load(Type *ty) {
	// in this case, we treat the arr as a pointer to the first ele in the array
	if (ty->kind == TY_ARR)
		return; // do nothing as address is already in rax
	
	if (ty->size == 1) // char type
		println("  movsbq (%%rax), %%rax");
	else
		println("  mov (%%rax), %%rax");
}

// type of store function dependent on type
static void store(Type *ty) {
	pop("%rdi\n"); // store top of stack into temp reg
	if (ty->size == 1)
		println("  mov %%al, (%%rdi)");
	else
		println("  mov %%rax, (%%rdi)"); // store val from reg into addr held in temp reg
}

// Gen code for an expression node
static void gen_expr(Node *node) {
	switch (node->kind) {
	case ND_NUM:
		println("  mov $%d, %%rax", node->val);
		return;
	case ND_NEG:
		gen_expr(node->lhs);
		println("  neg %%rax");
		return;
	case ND_VAR:
		gen_addr_and_load(node);
		// deref addr in rax and store val back in rax
		load(node->ty);
		return;
	case ND_DEREF:
		gen_expr(node->lhs);
		load(node->ty);
		return;
	case ND_ADDR:
		gen_addr_and_load(node->lhs);
		return;
	case ND_ASSIGN:
		gen_addr_and_load(node->lhs);
		push(); // push var addr into stack
		gen_expr(node->rhs);
		store(node->ty); // store val in rax into addr at top of stack
		return;
	case ND_FNCALL: {
		int num_args = 0;
		for (Node *arg = node->args; arg; arg = arg->next) {
			gen_expr(arg);
			push();
			num_args++;
		}
		// note the reverse order as we are popping from a stack
		for (int i=num_args - 1; i >= 0; i--)
			pop(argregs64[i]); // find out if this is always the case or can pop out to lsb's only

		// printf("  mov $0, %%rax\n"); // NOTE: find out reason for this line
		println("  call %s", node->fnname); // note requires fn to be defined somewhere
		return;
	}
	case ND_STMT_EXPR:
	for (Node *n = node->body; n; n = n->next)
		gen_stmt(n);
	return;
	default:;
	}

	// all other node types are binary
	gen_expr(node->rhs); // load rhs val into rax reg
	push(); // push rax reg val into stack
	gen_expr(node->lhs); // load lhs val into rax reg
	pop("%rdi");	// pop rhs val into rdi register
					// swapping with the line above fails as rdi val could be
					// overwritten as we codegen for child

	switch (node->kind) {
	case ND_ADD:
		println("  add %%rdi, %%rax"); // dest is rax
		return;
	case ND_SUB:
		println("  sub %%rdi, %%rax"); // sub source (rdi) from dest (rax)
		return;
	case ND_MUL:
		println("  imul %%rdi, %%rax"); // dest is rax
		return;
	case ND_DIV:
		println("  cqo"); // converts quadword in rax to octoword rdx:rax
		println("  idiv %%rdi"); // signed divide the octoword by rdi
		return;
	case ND_EQ:
	case ND_NE:
	case ND_LT:
	case ND_LTE:
		println("  cmp %%rdi, %%rax"); // sets flag code based on  $rax - $rdi (essentially cmp op2 op1 does comparison of op1 and op2 in that order)
		// $al is the least signficant byte of the ax register, set val accordingly
		if (node->kind == ND_EQ)
			println("  sete %%al");
		else if (node->kind == ND_NE)
			println("  setne %%al");
		else if (node->kind == ND_LT)
			println("  setl %%al"); // sets al to 1 if rax > rdi
		else
			println("  setle %%al"); // sets al to 1 if rax >= rdi

		println("  movzb %%al, %%rax");	
		return;
	// exact same handling as block as final expression statement places expression eval result into rax
	// which is what we expect from an expr eval
	default:;
	}

	error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
	switch (node->kind) {
	case ND_IF: { // blocks are used so c var wont intefere in other cases
		int c = count();
		gen_expr(node->cond);
		println("  cmp $0, %%rax");
		println("  je .L.else.%d", c); // append id to section to make it unique
		gen_stmt(node->then);
		println("  jmp .L.end.%d", c);
		println(".L.else.%d:", c);
		if (node->els) {
			gen_stmt(node->els);
		}
		println(".L.end.%d:", c);
		return;
	}
	case ND_FOR: {
		int c = count();
		if (node->init) {
			gen_stmt(node->init);
		}
		println(".L.begin.%d:", c);
		if (node->cond) {
			gen_expr(node->cond);
			println("  cmp $0, %%rax");
			println("  je .L.end.%d", c);
		}
		gen_stmt(node->then);
		if (node->inc) {
			gen_expr(node->inc);
		}
		println("  jmp .L.begin.%d", c);
		println(".L.end.%d:", c);
		return;
	}
	case ND_DOWHILE: {
		int c = count();
		println(".L.dowhile.%d:", c);
		gen_stmt(node->then);
		gen_expr(node->cond);
		println("  cmp $0, %%rax");
		println("  jne .L.dowhile.%d", c);
		return;
	}
	case ND_BLOCK:
		for (Node *n = node->body; n; n=n->next) {
			gen_stmt(n);
		}
		return;
	case ND_RETURN:
		gen_expr(node->lhs);
		println("  jmp .L.return.%s", current_fn->name);
		return;
	case ND_EXPR_STMT:
		gen_expr(node->lhs);
		return;
	default:;
	}
	error_tok(node->tok, "invalid statement");
}

// for each function we are calculating this for we are assigning for
// all the local variables in it (in a single stack frame). We are traversing
// the linked list of variables generated during parsing to do this.
static void assign_lvar_offsets(Obj *prog) {
	for (Obj *fn = prog; fn; fn = fn->next) {
		// skip global vars here
		if (!fn->is_function)
			continue;
		int offset = 0;
		for (Obj *var = fn->locals; var; var = var->next) {
			offset += var->ty->size;
			var->offset = -offset;
		}
		fn->stack_size = align_to(offset, 16); // possibly a requirement for alignment purposes, need to check c/x64 docs
	}
}

// code for global vars
static void emit_data(Obj *prog) {
	for (Obj *var = prog; var; var = var->next) {
		if (var->is_function)
			continue;
		println("  .data");
		println("  .globl %s", var->name);
		println("%s:", var->name);

		// one of those anonymous global vars used for string literals
		if (var->init_data) {
			for (int i = 0; i < var->ty->size; i++)
				println("  .byte %d", var->init_data[i]); // print char by char in numerical fmt
		} else
			println("  .zero %d", var->ty->size);
	}
}

// code for emitting function defs
static void emit_text(Obj *prog) {
	for (Obj *fn = prog; fn; fn = fn->next) {
		if (!fn->is_function)
			continue;
		println("  .globl %s", fn->name);
		println("  .text");
		println("%s:", fn->name);
		current_fn = fn;
		// Prologue
		println("  push %%rbp"); // save base pointer
		println("  mov %%rsp, %%rbp"); // set base pointer to stack ptr
		println("  sub $%d, %%rsp", fn->stack_size); // allocate space for variables
		int i = 0;
		for (Obj *var = fn->params; var; var = var->next) {
			if (var->ty->size == 1)
				println("  mov %s, %d(%%rbp)", argregs8[i++], var->offset);
			else
				println("  mov %s, %d(%%rbp)", argregs64[i++], var->offset);
		}

		// emit code for each function
		gen_stmt(fn->body);
		assert(depth == 0);

		// epilogue
		println(".L.return.%s:", fn->name); // provide section to jump to for return stmts
		println("  mov %%rbp, %%rsp");
		println("  pop %%rbp");
		println("  ret");
	}

}

void codegen(Obj *prog, FILE *out) {
	output_file = out;
	assign_lvar_offsets(prog);
	emit_data(prog);
	emit_text(prog);
}