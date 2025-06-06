#include <iostream>
#include <string>
#include <vector>
#include <memory>

using namespace std;

enum TermType { VAR, ABS, APP, NUM, BINOP };
enum BinOpKind { ADD, SUB, MUL };

struct Term {
    TermType type;
    string var_name;
    shared_ptr<Term> left, right, body;
    long long num_val;
    BinOpKind op;
    
    Term(TermType t) : type(t), num_val(0), op(ADD) {}
};

using TermPtr = shared_ptr<Term>;

inline vector<string> tokenize(const string& input) {
    vector<string> result;
    int i = 0, n = input.length();
    
    while (i < n) {
        char c = input[i];
        
        if (c <= ' ') {
            i++;
            continue;
        }
        
        if (c == '(' || c == ')' || c == '\\' || c == '.' || 
            c == '+' || c == '-' || c == '*') {
            result.push_back(string(1, c));
            i++;
        } else if (c >= '0' && c <= '9') {
            string num;
            while (i < n && input[i] >= '0' && input[i] <= '9') {
                num += input[i++];
            }
            result.push_back(num);
        } else {
            string var;
            while (i < n && ((input[i] >= 'a' && input[i] <= 'z') || 
                            (input[i] >= 'A' && input[i] <= 'Z') ||
                            (input[i] >= '0' && input[i] <= '9') || input[i] == '_')) {
                var += input[i++];
            }
            result.push_back(var);
        }
    }
    
    return result;
}

inline bool is_binop(const string& token) {
    return token == "+" || token == "-" || token == "*";
}

TermPtr parse_expr(const vector<string>& tokens, int& pos);

inline TermPtr parse_atom(const vector<string>& tokens, int& pos) {
    if (tokens[pos] == "(") {
        pos++;
        auto term = parse_expr(tokens, pos);
        pos++; // skip ')'
        return term;
    } else if (tokens[pos] == "\\") {
        pos++;
        string var = tokens[pos++];
        pos++; // skip '.'
        
        auto body = parse_expr(tokens, pos);
        auto abs_term = make_shared<Term>(ABS);
        abs_term->var_name = var;
        abs_term->body = body;
        return abs_term;
    } else if (tokens[pos][0] >= '0' && tokens[pos][0] <= '9') {
        auto num_term = make_shared<Term>(NUM);
        num_term->num_val = stoll(tokens[pos++]);
        return num_term;
    } else {
        auto var_term = make_shared<Term>(VAR);
        var_term->var_name = tokens[pos++];
        return var_term;
    }
}

inline TermPtr parse_term(const vector<string>& tokens, int& pos) {
    auto term = parse_atom(tokens, pos);
    
    while (pos < tokens.size() && 
           tokens[pos] != ")" && 
           tokens[pos] != "." &&
           !is_binop(tokens[pos])) {
        auto arg = parse_atom(tokens, pos);
        
        auto app_term = make_shared<Term>(APP);
        app_term->left = term;
        app_term->right = arg;
        term = app_term;
    }
    
    return term;
}

inline TermPtr parse_mul(const vector<string>& tokens, int& pos) {
    auto left = parse_term(tokens, pos);
    
    while (pos < tokens.size() && tokens[pos] == "*") {
        pos++;
        auto right = parse_term(tokens, pos);
        
        auto binop_term = make_shared<Term>(BINOP);
        binop_term->left = left;
        binop_term->right = right;
        binop_term->op = MUL;
        left = binop_term;
    }
    
    return left;
}

inline TermPtr parse_add(const vector<string>& tokens, int& pos) {
    auto left = parse_mul(tokens, pos);
    
    while (pos < tokens.size()) {
        BinOpKind op;
        if (tokens[pos] == "+") {
            op = ADD;
        } else if (tokens[pos] == "-") {
            op = SUB;
        } else {
            break;
        }
        pos++;
        
        auto right = parse_mul(tokens, pos);
        
        auto binop_term = make_shared<Term>(BINOP);
        binop_term->left = left;
        binop_term->right = right;
        binop_term->op = op;
        left = binop_term;
    }
    
    return left;
}

inline TermPtr parse_expr(const vector<string>& tokens, int& pos) {
    return parse_add(tokens, pos);
}

inline TermPtr parse(const string& input) {
    auto tokens = tokenize(input);
    int pos = 0;
    return parse_expr(tokens, pos);
}

TermPtr substitute(TermPtr term, const string& var, TermPtr replacement) {
    switch (term->type) {
        case VAR:
            return (term->var_name == var) ? replacement : term;
            
        case NUM:
            return term;
            
        case ABS:
            if (term->var_name == var) {
                return term;
            } else {
                auto new_abs = make_shared<Term>(ABS);
                new_abs->var_name = term->var_name;
                new_abs->body = substitute(term->body, var, replacement);
                return new_abs;
            }
            
        case APP: {
            auto new_app = make_shared<Term>(APP);
            new_app->left = substitute(term->left, var, replacement);
            new_app->right = substitute(term->right, var, replacement);
            return new_app;
        }
        
        case BINOP: {
            auto new_binop = make_shared<Term>(BINOP);
            new_binop->left = substitute(term->left, var, replacement);
            new_binop->right = substitute(term->right, var, replacement);
            new_binop->op = term->op;
            return new_binop;
        }
    }
    return term;
}

TermPtr evaluate(TermPtr term) {
    switch (term->type) {
        case VAR:
        case NUM:
            return term;
            
        case ABS: {
            auto new_abs = make_shared<Term>(ABS);
            new_abs->var_name = term->var_name;
            new_abs->body = evaluate(term->body);
            return new_abs;
        }
        
        case APP: {
            auto func = evaluate(term->left);
            auto arg = evaluate(term->right);
            
            if (func->type == ABS) {
                auto substituted = substitute(func->body, func->var_name, arg);
                return evaluate(substituted);
            } else {
                auto new_app = make_shared<Term>(APP);
                new_app->left = func;
                new_app->right = arg;
                return new_app;
            }
        }
        
        case BINOP: {
            auto left = evaluate(term->left);
            auto right = evaluate(term->right);
            
            if (left->type == NUM && right->type == NUM) {
                long long result;
                switch (term->op) {
                    case ADD: result = left->num_val + right->num_val; break;
                    case SUB: result = left->num_val - right->num_val; break;
                    case MUL: result = left->num_val * right->num_val; break;
                }
                auto num_term = make_shared<Term>(NUM);
                num_term->num_val = result;
                return num_term;
            } else {
                auto new_binop = make_shared<Term>(BINOP);
                new_binop->left = left;
                new_binop->right = right;
                new_binop->op = term->op;
                return new_binop;
            }
        }
    }
    return term;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    
    int n;
    cin >> n;
    cin.ignore();
    
    for (int i = 0; i < n; i++) {
        string line;
        getline(cin, line);
        
        auto term = parse(line);
        auto result = evaluate(term);
        cout << result->num_val << '\n';
    }
    
    return 0;
}