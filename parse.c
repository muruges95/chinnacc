#include "chinnacc.h"

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

// This will be delving into actuating the translation scheme
// Need to write it down before adopting it
// Currently the rest that is being passed down not being used, likely for 
// debugging purposes in future commits
static Node *expr(Token *tok, Token **rest);
static Node *equality(Token *tok, Token **rest);
static Node *relational(Token *tok, Token **rest);
static Node *add(Token *tok, Token **rest);
static Node *mul(Token *tok, Token **rest);
static Node *unary(Token *tok, Token **rest);
static Node *primary(Token *tok, Token **rest);

// primary: the units that are unbreakable, start with this,
// will also include the non-terminal involved in the lowest precedence ops
// Translation scheme: primary -> digit | "(" expr ")"
static Node *primary(Token *tok, Token **rest) {
	if (tok->kind == TK_NUM) {
		Node *node = new_numeric_node(tok->val);
		*rest = tok->next;
		return node;
	}

	if (equal(tok, "(")) {
		Node *node = expr(tok->next, &tok);
		*rest = skip(tok, ")");
		return node;
	}

	error_tok(tok, "expected either a number or `(`");
}

// sink directly into lowest priority level
static Node *expr(Token *tok, Token **rest) {
	return equality(tok, rest);
}

// equality: in this case the translation scheme targeting the operators with the 
// lowest precedence, will loop back here from the unbreakable (highest preced
// level) in case there is more to unpack. Its also the entry point non term.
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

Node *parse(Token *tok) {
	Node *node = expr(tok, &tok);
	// if not all tokens consumed when generating parse tree
	if (tok->kind != TK_EOF) {
		error_tok(tok, "extra token not parsed");
	}
	return node;
}
