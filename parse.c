#include "chinnacc.h"

// All local variable instances created during parsing are stored in a linked list
Obj *locals; // stack DS

static Node *new_node(NodeKind kind) {
	Node *node = calloc(1, sizeof(Node));
	node->kind = kind;
	return node;
}

// We include the nodekind here to allow for other unary ops in the future
// such as increment and decrement
static Node *new_unary_node(NodeKind kind, Node *expr) {
	Node *node = new_node(kind);
	node->lhs = expr;
	return node;
}

static Node *new_binary_node(NodeKind kind, Node *lhs, Node *rhs) {
	Node *node = new_node(kind);
	node->lhs = lhs;
	node->rhs = rhs;
	return node;
}

static Node *new_numeric_node(int val) {
	Node *node = new_node(ND_NUM);
	node->val = val;
	return node;
}

static Node *new_var_node(Obj *var) {
	Node *node = new_node(ND_VAR);
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
// Currently the rest that is being passed down not being used, likely for 
// debugging purposes in future commits
// EXPRESSIONS
static Node *expr(Token *tok, Token **rest);
static Node *assign(Token *tok, Token **rest);
static Node *equality(Token *tok, Token **rest);
static Node *relational(Token *tok, Token **rest);
static Node *add(Token *tok, Token **rest);
static Node *mul(Token *tok, Token **rest);
static Node *unary(Token *tok, Token **rest);
static Node *primary(Token *tok, Token **rest);

// STATEMENTS
static Node *stmt(Token *tok, Token **rest);
static Node *expr_stmt(Token *tok, Token **rest);

// primary: the units that are unbreakable, start with this,
// will also include the non-terminal involved in the lowest precedence ops
// Translation scheme: primary -> "(" expr ")" | ident | num
static Node *primary(Token *tok, Token **rest) {
	if (equal(tok, "(")) {
		Node *node = expr(tok->next, &tok);
		*rest = skip(tok, ")");
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
		*rest = tok->next;
		return new_var_node(var);
	}

	if (tok->kind == TK_NUM) {
		Node *node = new_numeric_node(tok->val);
		*rest = tok->next;
		return node;
	}

	error_tok(tok, "expected either a number or `(`");
}

// sink directly into lowest priority level
static Node *expr(Token *tok, Token **rest) {
	return assign(tok, rest);
}

// in this case the translation scheme targeting the operators with the 
// lowest precedence, will loop back here from the unbreakable (highest preced
// level) in case there is more to unpack. Its also the entry point non term.
// Normal: assign -> equality "=" equality | equality "=" assign
// REGEX: assign -> equality ("=" assign)? (normal form more useful for recursion)
static Node *assign(Token *tok, Token **rest) {
	Node *node = equality(tok, &tok);
	
	if (equal(tok, "=")) {
		node = new_binary_node(ND_ASSIGN, node, assign(tok->next, &tok));
	}
	*rest = tok;
	return node;
}

// equality:
// follow doc for add on translation scheme, current lowest precedence level
static Node *equality(Token *tok, Token **rest) {
	Node *node = relational(tok, &tok);
	
	for (;;) {
		if (equal(tok, "==")) {
			node = new_binary_node(ND_EQ, node, relational(tok->next, &tok));
			continue;
		}
		if (equal(tok, "!=")) {
			node = new_binary_node(ND_NE, node, relational(tok->next, &tok));
			continue;
		}
		*rest = tok;
		return node;
	}
}

// follow doc for expr. Higher precedence that equality operators
static Node *relational(Token *tok, Token **rest) {
	Node *node = add(tok, &tok);

	for (;;) {
		if (equal(tok, "<")) {
			node = new_binary_node(ND_LT, node, add(tok->next, &tok));
			continue;
		}
		if (equal(tok, ">")) {
			node = new_binary_node(ND_LT, add(tok->next, &tok), node);
			continue;
		}
		if (equal(tok, "<=")) {
			node = new_binary_node(ND_LTE, node, add(tok->next, &tok));
			continue;
		}
		if (equal(tok, ">=")) {
			node = new_binary_node(ND_LTE, add(tok->next, &tok), node);
			continue;
		}
		*rest = tok;
		return node;
	}
}

// Translation scheme: add -> mul | add "+" mul | add "-" mul
// With regex (as per chibicc): add -> mul ("+" mul | "-" mul)* (using this)

static Node *add(Token *tok, Token **rest) {
	Node *node = mul(tok, &tok);

	/**
	 * Note to self, easier to do a one time parse for left associativity
	 * using iteration, whereas, it is easier to do one for right associativity
	 * using recursion (see assignment expr for an example)
	**/
	for (;;) {
		if (equal(tok, "+")) {
			node = new_binary_node(ND_ADD, node, mul(tok->next, &tok));
			continue;
		}
		if (equal(tok, "-")) {
			node = new_binary_node(ND_SUB, node, mul(tok->next, &tok));
			continue;
		}
		*rest = tok;
		return node; // if reached, exit the loop
	}
}

// mul, the next precedence level. notice how the prev one always has at least
// one non-terminal translation involving the next highest (inc some terminals)
// Translation scheme: mul -> unary | mul "*" unary | mul "/" unary
// With regex (as per chibicc): mul -> unary ("*" unary | "/" unary)*

static Node *mul(Token *tok, Token **rest) {
	Node *node = unary(tok, &tok);

	for (;;) {
		if (equal(tok, "*")) {
			node = new_binary_node(ND_MUL, node, unary(tok->next, &tok));
			continue;
		}
		if (equal(tok, "/")) {
			node = new_binary_node(ND_DIV, node, unary(tok->next, &tok));
			continue;
		}

		*rest = tok;
		return node; // if reached, exit the loop
	}
}

// unary, next precedence level
// Translation scheme: unary -> primary | "+" unary | "-" unary
// In this case the handling is slightly diff for all 3 cases

static Node *unary(Token *tok, Token **rest) {
	// ignore + unary
	if (equal(tok, "+")) {
		return unary(tok->next, rest);
	}
	if (equal(tok, "-")) {
		return new_unary_node(ND_NEG, unary(tok->next, rest));
	}

	return primary(tok, rest);
}

// stmt -> expr-stmt (more to come as we implmnt them)
static Node *stmt(Token *tok, Token **rest) {
	return expr_stmt(tok, rest);
}

// expr-stmt -> expr ";"
static Node *expr_stmt(Token *tok, Token **rest) {
	Node *node = new_unary_node(ND_EXPR_STMT, expr(tok, &tok));
	*rest = skip(tok, ";");
	return node;
}

// top level parsing translation scheme, equivalent to the following
// program = stmt*
Function *parse(Token *tok) {
	Node head = {};
	Node *cur = &head;
	while (tok->kind != TK_EOF) {
		cur = cur->next = stmt(tok, &tok);
	}
	
	Function *prog = calloc(1, sizeof(Function));
	prog->body = head.next;
	prog->locals = locals;
	return prog;
}
