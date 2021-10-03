#include "chinnacc.h"

// purpose of this file is to implement a basic type inference system to determine which versions of operators we are using (in the case of overloaded operators)

Type *ty_int = &(Type){TY_INT}; // address of obj typecasted to Type

bool is_integer(Type *ty) {
	return ty->kind == TY_INT;
}

Type *pointer_to(Type *base) {
	Type *ty = calloc(1, sizeof(Type));
	ty->kind = TY_PTR;
	ty->base = base;
	return ty;
}

// recursively adding type to a node and its descendants
void add_type(Node *node) {
	if (!node || node->ty)
		return;

	// to apply recursively to all the nodes we are storing ptrs to
	add_type(node->lhs);
	add_type(node->rhs);
	add_type(node->cond);
	add_type(node->then);
	add_type(node->els);
	add_type(node->init);
	add_type(node->inc);

	for (Node *n = node->body; n; n = n->next)
		add_type(n);

	switch (node->kind) {
		case ND_ADD:
		case ND_SUB:
		case ND_MUL:
		case ND_DIV:
		case ND_NEG:
		case ND_ASSIGN:
			// for these cases infer type based on child nodes
			node->ty = node->lhs->ty;
			return;
		case ND_EQ:
		case ND_NE:
		case ND_LT:
		case ND_LTE:
		case ND_VAR:
		case ND_NUM:
			node->ty = ty_int; // use a constant where we can to save space
			return;
		case ND_ADDR:
			node->ty = pointer_to(node->lhs->ty);
			return;
		case ND_DEREF:
			if (node->lhs->ty->kind == TY_PTR)
				node->ty = node->lhs->ty->base;
			else
				node->ty = ty_int; // this is just to catch this erroneous case and not stop the type inference
			return;
	}
}
