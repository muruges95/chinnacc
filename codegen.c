#include "chinnacc.h"

static int depth = 0; // stack depth
// conventional registers used for first 6 args of function in a fn call, in the correct order
static char *argregs[] = { "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9" };
static Function *current_fn; // stacktracee of fns to know where to jmp to

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

// for generating sections to jump to
static int count(void) {
	static int i = 1;
	return i++;
}

static void push(void) {
	printf("  push %%rax\n");
	depth++;
}

static void pop(char *arg) {
	printf("  pop %s\n", arg);
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
	if (node->kind == ND_VAR) {
	}
	switch (node->kind) {
		case ND_VAR:
			printf("  lea %d(%%rbp), %%rax\n", node->var->offset);
			return;
		case ND_DEREF:
			gen_expr(node->lhs);
			return;
	}

	error_tok(node->tok, "invalid expression");
}

// for each function we are calculating this for we are assigning for
// all the local variables in it (in a single stack frame). We are traversing
// the linked list of variables generated during parsing to do this.
static void assign_lvar_offsets(Function *prog) {
	for (Function *fn = prog; fn; fn = fn->next) {
		int offset = 0;
		// for each we assign 8 bytes of space (sufficient for ints and longs)
		for (Obj *var = fn->locals; var; var = var->next) {
			offset += 8;
			var->offset = -offset;
		}
		fn->stack_size = align_to(offset, 16); // possibly a requirement for alignment purposes, need to check c/x64 docs
	}
}

// Gen code for an expression node
static void gen_expr(Node *node) {
	switch (node->kind) {
		case ND_NUM:
			printf("  mov $%d, %%rax\n", node->val);
			return;
		case ND_NEG:
			gen_expr(node->lhs);
			printf("  neg %%rax\n");
			return;
		case ND_VAR:
			gen_addr_and_load(node);
			// deref addr in rax and store val back in rax
			printf("  mov (%%rax), %%rax\n");
			return;
		case ND_DEREF:
			gen_expr(node->lhs);
			printf("  mov (%%rax), %%rax\n");
			return;
		case ND_ADDR:
			gen_addr_and_load(node->lhs);
			return;
		case ND_ASSIGN:
			gen_addr_and_load(node->lhs);
			push(); // push var addr into stack
			gen_expr(node->rhs);
			pop("%rdi"); // pop var address into rdi
			printf("  mov %%rax, (%%rdi)\n"); // load rax into var addr in rdi
			return;
		case ND_FNCALL: {
			int num_args = 0;
			for (Node *arg = node->args; arg; arg = arg->next) {
				gen_expr(arg);
				push();
				num_args++;
			}
			for (int i=num_args - 1; i >= 0; i--)
				pop(argregs[i]);

			// printf("  mov $0, %%rax\n"); // NOTE: find out reason for this line
			printf("  call %s\n", node->fnname); // note requires fn to be defined somewhere
			return;
		}
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
		case ND_EQ:
		case ND_NE:
		case ND_LT:
		case ND_LTE:
			printf("  cmp %%rdi, %%rax\n"); // sets flag code based on  $rax - $rdi (essentially cmp op2 op1 does comparison of op1 and op2 in that order)
			// $al is the least signficant byte of the ax register, set val accordingly
			if (node->kind == ND_EQ)
				printf("  sete %%al\n");
			else if (node->kind == ND_NE)
				printf("  setne %%al\n");
			else if (node->kind == ND_LT)
				printf("  setl %%al\n"); // sets al to 1 if rax > rdi
			else
				printf("  setle %%al\n"); // sets al to 1 if rax >= rdi

			printf("  movzb %%al, %%rax\n");	
			return;
	}

	error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
	switch (node->kind) {
		case ND_IF: { // blocks are used so c var wont intefere in other cases
			int c = count();
			gen_expr(node->cond);
			printf("  cmp $0, %%rax\n");
			printf("  je .L.else.%d\n", c); // append id to section to make it unique
			gen_stmt(node->then);
			printf("  jmp .L.end.%d\n", c);
			printf(".L.else.%d:\n", c);
			if (node->els) {
				gen_stmt(node->els);
			}
			printf(".L.end.%d:\n", c);
			return;
		}
		case ND_FOR: {
			int c = count();
			if (node->init) {
				gen_stmt(node->init);
			}
			printf(".L.begin.%d:\n", c);
			if (node->cond) {
				gen_expr(node->cond);
				printf("  cmp $0, %%rax\n");
				printf("  je .L.end.%d\n", c);
			}
			gen_stmt(node->then);
			if (node->inc) {
				gen_expr(node->inc);
			}
			printf("  jmp .L.begin.%d\n", c);
			printf(".L.end.%d:\n", c);
			return;
		}
		case ND_DOWHILE: {
			int c = count();
			printf(".L.dowhile.%d:\n", c);
			gen_stmt(node->then);
			gen_expr(node->cond);
			printf("  cmp $0, %%rax\n");
			printf("  jne .L.dowhile.%d\n", c);
			return;
		}
		case ND_BLOCK:
			for (Node *n = node->body; n; n=n->next) {
				gen_stmt(n);
			}
			return;
		case ND_RETURN:
			gen_expr(node->lhs);
			printf(  "jmp .L.return.%s\n", current_fn->name);
			return;
		case ND_EXPR_STMT:
			gen_expr(node->lhs);
			return;
	}
	error_tok(node->tok, "invalid statement");
}

void codegen(Function *prog) {
	assign_lvar_offsets(prog);
	for (Function *fn = prog; fn; fn = fn->next) {
		printf("  .globl %s\n", fn->name);
		printf("%s:\n", fn->name);
		current_fn = fn;
		// Prologue
		printf("  push %%rbp\n"); // save base pointer
		printf("  mov %%rsp, %%rbp\n"); // set base pointer to stack ptr
		printf("  sub $%d, %%rsp\n", fn->stack_size); // allocate space for variables

		gen_stmt(fn->body);
		assert(depth == 0);

		// epilogue
		printf(".L.return.%s:\n", fn->name); // provide section to jump to for return stmts
		printf("  mov %%rbp, %%rsp\n");
		printf("  pop %%rbp\n");
		printf("  ret\n");
	}

}
