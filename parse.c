#include "chinnacc.h"

// All local variable instances created during parsing are stored in a linked list
Obj *locals; // stack DS

static Node *new_node(NodeKind kind, Token *tok) {
	Node *node = calloc(1, sizeof(Node));
	node->kind = kind;
	node->tok = tok;
	return node;
}

// We include the nodekind here to allow for other unary ops in the future
// such as increment and decrement
static Node *new_unary_node(NodeKind kind, Node *expr, Token *tok) {
	Node *node = new_node(kind, tok);
	node->lhs = expr;
	return node;
}

static Node *new_binary_node(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
	Node *node = new_node(kind, tok);
	node->lhs = lhs;
	node->rhs = rhs;
	return node;
}

static Node *new_numeric_node(int val, Token *tok) {
	Node *node = new_node(ND_NUM, tok);
	node->val = val;
	return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
	Node *node = new_node(ND_VAR, tok);
	node->var = var;
	return node;
}

// creates an Obj, meant for lvalues
static Obj *new_lvar(char *name, Type *ty) {
	Obj *var = calloc(1, sizeof(Obj));
	var->name = name;
	var->ty = ty;
	var->next = locals; // add to top of locals stack
	locals = var;
	// note offset not set here
	return var;
}

static char *get_ident(Token *tok) {
	// need to create obj, strndup is a posix fn for which we added the special header
	// essentially making a copy of the string and using it for the obj
	// NOTE TO SELF:see what happens if the pointer was passed directly via tok->loc
	if (tok->kind != TK_IDENT)
		error_tok(tok, "expected an identifier");
	return strndup(tok->loc, tok->len);
}

// find a local var by name from the linked list
static Obj *find_var(Token *tok) {
	for (Obj *var = locals; var; var = var->next) {
		if (strlen(var->name) == tok->len && !strncmp(var->name, tok->loc, tok->len)) {
			return var;
		}
	}
	return NULL;
}

// This will be delving into actuating the translation scheme
// Need to write it down before adopting it
// Currently the rest that is being passed is used to advance the pointer to the token stream outside of the current function
// EXPRESSIONS
static Node *expr(Token **tok_loc);
static Node *assign(Token **tok_loc);
static Node *equality(Token **tok_loc);
static Node *relational(Token **tok_loc);
static Node *add(Token **tok_loc);
static Node *mul(Token **tok_loc);
static Node *unary(Token **tok_loc);
static Node *primary(Token **tok_loc);

// STATEMENTS
static Node *declaration(Token **tok_loc);
static Node *stmt(Token **tok_loc);
static Node *expr_stmt(Token **tok_loc);
static Node *compound_stmt(Token **tok_loc);

// FUNCTION SPECIFIC
static Type *declspec(Token **tok_loc);
static Type *declarator(Token **tok_loc, Type *type);

// fncall -> ident "(" (assign ("," assign)*)? ")"
static Node *fncall(Token **tok_loc) {
	Token *tok = *tok_loc;
	*tok_loc = tok->next->next;

	Node head = {};
	Node *cur = &head;

	while (!consume(tok_loc, ")")) {
		if (cur != &head)
			*tok_loc = skip(*tok_loc, ",");
		cur = cur->next = assign(tok_loc);
	}

	Node *node = new_node(ND_FNCALL, tok);
	node->fnname = strndup(tok->loc, tok->len);
	node->args = head.next;
	return node;
}

// primary: the units that are unbreakable, start with this,
// will also include the non-terminal involved in the lowest precedence ops
// Translation scheme: primary -> "(" expr ")" | ident fn-args? | num
// Note that fncall handling is done separately
static Node *primary(Token **tok_loc) {
	if (consume(tok_loc, "(")) {
		Node *node = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		return node;
	}

	Token *tok = *tok_loc;
	if (tok->kind == TK_IDENT) {
		// Function call
		if (equal(tok->next, "(")) {
			return fncall(tok_loc);
		}

		// Variable
		// first try to find if obj has alr been defined for this identifier in stack
		Obj *var = find_var(tok);
		if (!var) {
			error_tok(tok, "undefined variable");
		}
		*tok_loc = tok->next;
		return new_var_node(var, tok);
	}

	if (tok->kind == TK_NUM) {
		Node *node = new_numeric_node(tok->val, tok);
		*tok_loc = tok->next;
		return node;
	}

	error_tok(tok, "expected either a number or `(`");
}

// sink directly into lowest priority level
static Node *expr(Token **tok_loc) {
	return assign(tok_loc);
}

// in this case the translation scheme targeting the operators with the 
// lowest precedence, will loop back here from the unbreakable (highest preced
// level) in case there is more to unpack. Its also the entry point non term.
// Normal: assign -> equality "=" equality | equality "=" assign
// REGEX: assign -> equality ("=" assign)? (normal form more useful for recursion)
static Node *assign(Token **tok_loc) {
	Node *node = equality(tok_loc);
	Token *tok = *tok_loc;
	
	if (consume(tok_loc, "=")) {
		return new_binary_node(ND_ASSIGN, node, assign(tok_loc), tok);
	}
	return node;
}

// equality:
// follow doc for add on translation scheme
static Node *equality(Token **tok_loc) {
	Node *node = relational(tok_loc);

	for (;;) {
		Token *tok = *tok_loc;
		if (consume(tok_loc, "==")) {
			node = new_binary_node(ND_EQ, node, relational(tok_loc), tok);
			continue;
		}
		if (consume(tok_loc, "!=")) {
			node = new_binary_node(ND_NE, node, relational(tok_loc), tok);
			continue;
		}
		return node;
	}
}

// follow doc for expr. Higher precedence that equality operators
static Node *relational(Token **tok_loc) {
	Node *node = add(tok_loc);

	for (;;) {
		Token *tok = *tok_loc;
		if (consume(tok_loc, "<")) {
			node = new_binary_node(ND_LT, node, add(tok_loc), tok);
			continue;
		}
		if (consume(tok_loc, ">")) {
			node = new_binary_node(ND_LT, add(tok_loc), node, tok);
			continue;
		}
		if (consume(tok_loc, "<=")) {
			node = new_binary_node(ND_LTE, node, add(tok_loc), tok);
			continue;
		}
		if (consume(tok_loc, ">=")) {
			node = new_binary_node(ND_LTE, add(tok_loc), node, tok);
			continue;
		}
		return node;
	}
}

// Implementation of the overloaded add and subtract operators
// for these, we need to do early type inference to know how to deal with the ops
// we are currently mul/div by 8 because that is the int size but this needs to eventually change to the size of the base types of the ptrs
static Node *new_add_node(Node *lhs, Node *rhs, Token *tok) {
	add_type(lhs);
	add_type(rhs);

	// int + int
	if (is_integer(lhs->ty) && is_integer(rhs->ty))
		return new_binary_node(ND_ADD, lhs, rhs, tok);
	
	// in  case of ptr + ptr
	if (lhs->ty->base && rhs->ty->base) {
		error_tok(tok, "invalid operand types");
	}

	// switch num + ptr to become ptr + num so type inference correctly infers ptr type as the result of the op
	
	if (!lhs->ty->base && rhs->ty->base) {
		Node *tmp = lhs;
		lhs = rhs;
		rhs = tmp;
	}

	// ptr + num
	rhs = new_binary_node(ND_MUL, rhs, new_numeric_node(8, tok), tok);
	return new_binary_node(ND_ADD, lhs, rhs, tok);
}

// similar to the overloaded add but we have to additionally handle when we have two ptrs

static Node *new_sub_node(Node *lhs, Node *rhs, Token *tok) {
	add_type(lhs);
	add_type(rhs);

	// int - int
	if (is_integer(lhs->ty) && is_integer(rhs->ty)) {
		return new_binary_node(ND_SUB, lhs, rhs, tok);
	}

	// in case of ptr - num
	if (lhs->ty->base && is_integer(rhs->ty)) {
		rhs = new_binary_node(ND_MUL, rhs, new_numeric_node(8, tok), tok);
		return new_binary_node(ND_SUB, lhs, rhs, tok);
	}

	// ptr - ptr
	if (lhs->ty->base && rhs->ty->base) {
		Node *node = new_binary_node(ND_SUB, lhs, rhs, tok);
		node->ty = ty_int;
		return new_binary_node(ND_DIV, node, new_numeric_node(8, tok), tok);
	}

	error_tok(tok, "invalid operand types");
}

// Translation scheme: add -> mul | add "+" mul | add "-" mul
// With regex (as per chibicc): add -> mul ("+" mul | "-" mul)* (using this)

static Node *add(Token **tok_loc) {
	Node *node = mul(tok_loc);

	/**
	 * Note to self, easier to do a one time parse for left associativity
	 * using iteration, whereas, it is easier to do one for right associativity
	 * using recursion (see assignment expr for an example)
	**/
	for (;;) {
		Token *tok = *tok_loc;
		if (consume(tok_loc, "+")) {
			node = new_add_node(node, mul(tok_loc), tok);
			continue;
		}
		if (consume(tok_loc, "-")) {
			node = new_sub_node(node, mul(tok_loc), tok);
			continue;
		}
		return node; // if reached, exit the loop
	}
}

// mul, the next precedence level. notice how the prev one always has at least
// one non-terminal translation involving the next highest (inc some terminals)
// Translation scheme: mul -> unary | mul "*" unary | mul "/" unary
// With regex (as per chibicc): mul -> unary ("*" unary | "/" unary)*

static Node *mul(Token **tok_loc) {
	Node *node = unary(tok_loc);

	for (;;) {
		Token *tok = *tok_loc;
		if (consume(tok_loc, "*")) {
			node = new_binary_node(ND_MUL, node, unary(tok_loc), tok);
			continue;
		}
		if (consume(tok_loc, "/")) {
			node = new_binary_node(ND_DIV, node, unary(tok_loc), tok);
			continue;
		}

		return node; // if reached, exit the loop
	}
}

// unary, next precedence level
// Translation scheme: unary -> primary | "+" unary | "-" unary | "*" unary | "&" unary
// In this case the handling is slightly diff for all 3 cases

static Node *unary(Token **tok_loc) {
	// ignore + unary
	Token *tok = *tok_loc;
	if (consume(tok_loc, "+")) {
		return unary(tok_loc);
	}
	if (consume(tok_loc, "-")) {
		return new_unary_node(ND_NEG, unary(tok_loc), tok);
	}
	if (consume(tok_loc, "&")) {
		return new_unary_node(ND_ADDR, unary(tok_loc), tok);
	}
	if (consume(tok_loc, "*")) {
		return new_unary_node(ND_DEREF, unary(tok_loc), tok);
	}

	return primary(tok_loc);
}

// stmt -> "return" expr ";" // we dont use expr-stmt as what we want to return is just the expr, so that should be the child node
//      | "for" "(" expr-stmt expr? ";" expr? ")" // stmt we dont use an expr-stmt for the conditional as we want our conditional to be an expression not a stmt, just like while and if
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "{" compound-stmt (for blocks)
//      | expr-stmt
static Node *stmt(Token **tok_loc) {
	Token *tok = *tok_loc;
	if (consume(tok_loc, "return")) {
		Node *node = new_node(ND_RETURN, tok);
		node->lhs = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ";");
		return node;
	}

	if (consume(tok_loc, "for")) {
		Node *node = new_node(ND_FOR, tok);
		*tok_loc = skip(*tok_loc, "(");
		node->init = expr_stmt(tok_loc);
		if (!equal(*tok_loc, ";")) {
			node->cond = expr(tok_loc);
		}
		*tok_loc = skip(*tok_loc, ";");
		if (!equal(*tok_loc, ")")) {
			node->inc = expr(tok_loc);
		}
		*tok_loc = skip(*tok_loc, ")");
		node->then = stmt(tok_loc);
		return node;
	}

	if (consume(tok_loc, "if")) {
		Node *node = new_node(ND_IF, tok);
		*tok_loc = skip(*tok_loc, "(");
		node->cond = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		node->then = stmt(tok_loc);
		if (consume(tok_loc, "else")) {
			node->els = stmt(tok_loc);
		}
		return node;
	}

	if (consume(tok_loc, "while")) {
		Node *node = new_node(ND_FOR, tok);
		*tok_loc = skip(*tok_loc, "(");
		node->cond = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		node->then = stmt(tok_loc);
		return node;
	}

	if (consume(tok_loc, "do")) {
		Node *node = new_node(ND_DOWHILE, tok);
		node->then = stmt(tok_loc);
		*tok_loc = skip(*tok_loc, "while");
		*tok_loc = skip(*tok_loc, "(");
		node->cond = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		*tok_loc = skip(*tok_loc, ";");
		return node;
	}

	if (equal(tok, "{")) {
		*tok_loc = tok->next;
		return compound_stmt(tok_loc);
	}
	return expr_stmt(tok_loc);
}

// compound-stmt -> (declaration | stmt)* "}"
static Node *compound_stmt(Token **tok_loc) {
	Node head = {};
	Node *body_node = &head;
	Node *node = new_node(ND_BLOCK, *tok_loc);
	while (!equal(*tok_loc, "}")) {
		if (equal(*tok_loc, "int"))
			body_node = body_node->next = declaration(tok_loc);
		else
			body_node = body_node->next = stmt(tok_loc);
		add_type(body_node);
	}
	*tok_loc = (*tok_loc)->next;
	node->body = head.next;
	return node;
}

// declaration and helper fns

// declspec = "int"
static Type *declspec(Token **tok_loc) {
	*tok_loc = skip(*tok_loc, "int");
	return ty_int;
}

// type suffix represents the types of the args in declaration
// type-suffix = ("(" func-params)?
static Type *type_suffix(Token **tok_loc, Type *ty) {
	if (consume(tok_loc, "(")) {
		*tok_loc = skip(*tok_loc, ")");
		return fn_type(ty);
	}
	return ty;
}

// declarator = "*"* ident type-suffix
static Type *declarator(Token **tok_loc, Type *ty) {
	while (consume(tok_loc, "*"))
		ty = pointer_to(ty);

	Token *tok = *tok_loc;

	if (tok->kind != TK_IDENT)
		error_tok(tok, "expected a variable name");
	*tok_loc = tok->next;
	ty = type_suffix(tok_loc, ty);
	ty->name = tok;
	return ty;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **tok_loc) {
	Type *basety = declspec(tok_loc);

	Node head = {};
	Node *cur = &head;
	int i = 0;

	while (!equal(*tok_loc, ";")) {
		// if multiple decl, skip the commas
		if (i++ > 0)
			*tok_loc = skip(*tok_loc, ",");

		Type *ty = declarator(tok_loc, basety);
		Obj *var = new_lvar(get_ident(ty->name), ty);

		if (!consume(tok_loc, "="))
			continue;

		Node *lhs = new_var_node(var, ty->name);
		Node *rhs = assign(tok_loc);
		Node *node = new_binary_node(ND_ASSIGN, lhs, rhs, *tok_loc);
		cur = cur->next = new_unary_node(ND_EXPR_STMT, node, *tok_loc);
	}

	Node *node = new_node(ND_BLOCK, *tok_loc);
	node->body = head.next;
	*tok_loc = (*tok_loc)->next;
	return node;
}

static Function *function(Token **tok_loc) {
	Type *ty = declspec(tok_loc);
	ty = declarator(tok_loc, ty);

	locals = NULL; // reset locals in function to null, and will be built up over succesive calls to append to the locals list

	Function *fn = calloc(1, sizeof(Function));
	fn->name = get_ident(ty->name);
	*tok_loc = skip(*tok_loc, "{");

	fn->body = compound_stmt(tok_loc);
	fn->locals = locals;
	fn->stack_size = 0;
	return fn;
}

// expr-stmt -> expr? ";"
static Node *expr_stmt(Token **tok_loc) {
	Token *tok = *tok_loc;
	if (equal(tok, ";")) {
		*tok_loc = tok->next;
		return new_node(ND_BLOCK, tok); // treat as empty block, compiler currently does not gen code for empty blocks
	}
	Node *node = new_node(ND_EXPR_STMT, tok);
	node->lhs = expr(tok_loc);
	*tok_loc = skip(*tok_loc, ";");
	return node;
}

// top level parsing translation scheme, equivalent to the following
// program = function-definition*
Function *parse(Token *tok) {
	Function head = {};
	Function *curr = &head;
	while (tok->kind != TK_EOF)
		curr = curr->next = function(&tok);
	return head.next;
}
