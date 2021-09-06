#include "chinnacc.h"

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
	switch (node->kind) {
		case ND_NUM:
			printf("  mov $%d, %%rax\n", node->val);
			return;
		case ND_NEG:
			gen_expr(node->lhs);
			printf("  neg %%rax\n");
			return;
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

	error("invalid expression");
}

static void gen_stmt(Node *node) {
	if (node->kind == ND_EXPR_STMT) {
		gen_expr(node->lhs);
		return;
	}
	error("invalid statement");
}

void codegen(Node *node) {
	printf("  .globl main\n");
	printf("main:\n");

	// codegen as we walk down parse tree
	for (Node *n = node; n; n = n->next) {
		gen_stmt(n);
		assert(depth == 0); // stack should be cleared after each stmt at top lvl
	}
	printf("  ret\n");
}
