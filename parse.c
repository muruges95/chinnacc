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
static Obj *new_lvar(char *name) {
	Obj *var = calloc(1, sizeof(Obj));
	var->name = name;
	var->next = locals; // add to top of locals stack
	locals = var;
	// note offset not set here
	return var;
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
static Node *stmt(Token **tok_loc);
static Node *expr_stmt(Token **tok_loc);
static Node *compound_stmt(Token **tok_loc);

// primary: the units that are unbreakable, start with this,
// will also include the non-terminal involved in the lowest precedence ops
// Translation scheme: primary -> "(" expr ")" | ident | num
static Node *primary(Token **tok_loc) {
	Token *tok = *tok_loc;
	if (equal(tok, "(")) {
		*tok_loc = tok->next;
		Node *node = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		return node;
	}

	if (tok->kind == TK_IDENT) {
		// first try to find if obj has alr been defined for this identifier in stack
		Obj *var = find_var(tok);
		if (!var) {
			// need to create obj, strndup is a posix fn for which we added the special header
			// essentially making a copy of the string and using it for the obj
			// NOTE TO SELF:see what happens if the pointer was passed directly via tok->loc
			var = new_lvar(strndup(tok->loc, tok->len));
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
	
	if (equal(tok, "=")) {
		*tok_loc = tok->next;
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
		if (equal(tok, "==")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_EQ, node, relational(tok_loc), tok);
			continue;
		}
		if (equal(tok, "!=")) {
			*tok_loc = tok->next;
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
		if (equal(tok, "<")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_LT, node, add(tok_loc), tok);
			continue;
		}
		if (equal(tok, ">")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_LT, add(tok_loc), node, tok);
			continue;
		}
		if (equal(tok, "<=")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_LTE, node, add(tok_loc), tok);
			continue;
		}
		if (equal(tok, ">=")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_LTE, add(tok_loc), node, tok);
			continue;
		}
		return node;
	}
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
		if (equal(tok, "+")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_ADD, node, mul(tok_loc), tok);
			continue;
		}
		if (equal(tok, "-")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_SUB, node, mul(tok_loc), tok);
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
		if (equal(tok, "*")) {
			*tok_loc = tok->next;
			node = new_binary_node(ND_MUL, node, unary(tok_loc), tok);
			continue;
		}
		if (equal(tok, "/")) {
			*tok_loc = tok->next;
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
	if (equal(tok, "+")) {
		*tok_loc = tok->next;
		return unary(tok_loc);
	}
	if (equal(tok, "-")) {
		*tok_loc = tok->next;
		return new_unary_node(ND_NEG, unary(tok_loc), tok);
	}
	if (equal(tok, "&")) {
		*tok_loc = tok->next;
		return new_unary_node(ND_ADDR, unary(tok_loc), tok);
	}
	if (equal(tok, "*")) {
		*tok_loc = tok->next;
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
	if (equal(tok, "return")) {
		Node *node = new_node(ND_RETURN, tok);
		*tok_loc = tok->next;
		node->lhs = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ";");
		return node;
	}

	if (equal(tok, "for")) {
		Node *node = new_node(ND_FOR, tok);
		*tok_loc = skip(tok->next, "(");
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

	if (equal(tok, "if")) {
		Node *node = new_node(ND_IF, tok);
		*tok_loc = skip(tok->next, "(");
		node->cond = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		node->then = stmt(tok_loc);
		if (equal(*tok_loc, "else")) {
			*tok_loc = (*tok_loc)->next;
			node->els = stmt(tok_loc);
		}
		return node;
	}

	if (equal(tok, "while")) {
		Node *node = new_node(ND_FOR, tok);
		*tok_loc = skip(tok->next, "(");
		node->cond = expr(tok_loc);
		*tok_loc = skip(*tok_loc, ")");
		node->then = stmt(tok_loc);
		return node;
	}

	if (equal(tok, "do")) {
		Node *node = new_node(ND_DOWHILE, tok);
		*tok_loc = tok->next;
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

// compound-stmt -> stmt* "}"
static Node *compound_stmt(Token **tok_loc) {
	Node head = {};
	Node *body_node = &head;
	Node *node = new_node(ND_BLOCK, *tok_loc);
	while (!equal(*tok_loc, "}")) {
		body_node = body_node->next = stmt(tok_loc);
	}
	*tok_loc = (*tok_loc)->next;
	node->body = head.next;
	return node;
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
// program = stmt*
Function *parse(Token *tok) {
	tok = skip(tok, "{");
	Function *prog = calloc(1, sizeof(Function));
	prog->body = compound_stmt(&tok);
	prog->locals = locals;
	return prog;
}
