#include "chinnacc.h"

// purpose of this file is to implement a basic type inference system to determine which versions of operators we are using (in the case of overloaded operators)

Type *ty_int = &(Type){TY_INT, 8}; // address of obj typecasted to Type

bool is_integer(Type *ty) {
	return ty->kind == TY_INT;
}

Type *pointer_to(Type *base) {
	Type *ty = calloc(1, sizeof(Type));
	ty->kind = TY_PTR;
	ty->size = 8;
	ty->base = base;
	return ty;
}

Type *arr_of(Type *base, int size) {
	Type *ty = calloc(1, sizeof(Type));
	ty->kind = TY_ARR;
	ty->size = 8 * size;
	ty->base = base;
	ty->array_len = size;
}

Type *fn_type(Type *return_ty) {
	Type *ty = calloc(1, sizeof(Type));
	ty->kind = TY_FN;
	ty->return_ty = return_ty;
	return ty;
}

Type *copy_type(Type *ty) {
	Type *copied_ty = calloc(1, sizeof(Type));
	*copied_ty = *ty;
	return copied_ty;
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

	for (Node *n = node->args; n; n = n->next)
		add_type(n);

	switch (node->kind) {
		case ND_ADD:
		case ND_SUB:
		case ND_MUL:
		case ND_DIV:
		case ND_NEG:
			// for these cases infer type based on child nodes
			node->ty = node->lhs->ty;
			return;
		case ND_ASSIGN:
			if (node->lhs->ty->kind == TY_ARR)
				error_tok(node->lhs->tok, "not an lval");
			node->ty = node->lhs->ty;
			return;
		case ND_EQ:
		case ND_NE:
		case ND_LT:
		case ND_LTE:
		case ND_NUM:
		case ND_FNCALL:
			node->ty = ty_int; // use a constant where we can to save space
			return;
		case ND_VAR:
			node->ty = node->var->ty;
			return;
		case ND_ADDR:
			if (node->lhs->ty->kind == TY_ARR)
				// In this case a array var is an alias of its address (simply put)
				// thats why it has the same type as a pointer of the base type of the
				// arr instead of a double ptr
				node->ty = pointer_to(node->lhs->ty->base);
			else
				node->ty = pointer_to(node->lhs->ty);
			return;
		case ND_DEREF:
			if (!node->lhs->ty->base) // not ptr nor arr
				error_tok(node->tok, "invalid pointer dereference");
			node->ty = node->lhs->ty->base;
			return;
	}
}

