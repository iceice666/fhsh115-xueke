use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io::{self, BufRead};

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(String),
    Abs(String, Box<Term>),  // λvar.body
    App(Box<Term>, Box<Term>), // function application
    Num(i64), // Numeric literals
    BinOp(Box<Term>, BinOpKind, Box<Term>), // Binary operations
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(name) => write!(f, "{}", name),
            Term::Abs(var, body) => write!(f, "(\\{}.{})", var, body),
            Term::App(func, arg) => write!(f, "({} {})", func, arg),
            Term::Num(n) => write!(f, "{}", n),
            Term::BinOp(left, op, right) => {
                let op_str = match op {
                    BinOpKind::Add => "+",
                    BinOpKind::Sub => "-",
                    BinOpKind::Mul => "*",
                };
                write!(f, "({} {} {})", left, op_str, right)
            }
        }
    }
}

pub struct LambdaCalculusSolver;

impl LambdaCalculusSolver {
    pub fn new() -> Self {
        LambdaCalculusSolver
    }

    /// Parse a lambda expression from string format
    pub fn parse(&self, input: &str) -> Result<Term, String> {
        let tokens = self.tokenize(input)?;
        let mut pos = 0;
        self.parse_term(&tokens, &mut pos)
    }

    /// Evaluate a lambda expression using beta reduction and arithmetic
    pub fn evaluate(&self, term: &Term) -> Term {
        self.beta_reduce(term)
    }

    /// Check if two terms are alpha-equivalent (same up to variable renaming)
    pub fn alpha_equivalent(&self, term1: &Term, term2: &Term) -> bool {
        self.alpha_equiv_helper(term1, term2, &mut HashMap::new(), &mut HashMap::new())
    }

    /// Get free variables in a term
    pub fn free_variables(&self, term: &Term) -> HashSet<String> {
        self.free_vars_helper(term, &HashSet::new())
    }

    /// Substitute variable with term
    pub fn substitute(&self, term: &Term, var: &str, replacement: &Term) -> Term {
        self.subst_helper(term, var, replacement, &HashSet::new())
    }

    // Private helper methods

    fn tokenize(&self, input: &str) -> Result<Vec<String>, String> {
        let mut tokens = Vec::new();
        let mut chars = input.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                ' ' | '\t' | '\n' => { chars.next(); }
                '(' | ')' | '\\' | '.' | '+' | '-' | '*' => {
                    tokens.push(chars.next().unwrap().to_string());
                }
                c if c.is_ascii_digit() => {
                    let mut num = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit() {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(num);
                }
                c if c.is_alphabetic() => {
                    let mut name = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(name);
                }
                _ => return Err(format!("Unexpected character: {}", ch)),
            }
        }
        Ok(tokens)
    }

    fn parse_term(&self, tokens: &[String], pos: &mut usize) -> Result<Term, String> {
        if *pos >= tokens.len() {
            return Err("Unexpected end of input".to_string());
        }

        let term = match tokens[*pos].as_str() {
            "(" => {
                *pos += 1;
                let term = self.parse_expression(tokens, pos)?;
                if *pos >= tokens.len() || tokens[*pos] != ")" {
                    return Err("Expected closing parenthesis".to_string());
                }
                *pos += 1;
                term
            }
            "\\" => {
                *pos += 1;
                if *pos >= tokens.len() {
                    return Err("Expected variable after λ".to_string());
                }
                let var = tokens[*pos].clone();
                *pos += 1;
                if *pos >= tokens.len() || tokens[*pos] != "." {
                    return Err("Expected '.' after variable in λ-abstraction".to_string());
                }
                *pos += 1;
                let body = self.parse_expression(tokens, pos)?;
                Term::Abs(var, Box::new(body))
            }
            token if token.chars().all(|c| c.is_ascii_digit()) => {
                *pos += 1;
                Term::Num(token.parse().map_err(|_| "Invalid number")?)
            }
            var if var.chars().all(|c| c.is_alphanumeric() || c == '_') => {
                *pos += 1;
                Term::Var(var.to_string())
            }
            _ => return Err(format!("Unexpected token: {}", tokens[*pos])),
        };

        // Handle applications (left-associative)
        let mut result = term;
        while *pos < tokens.len() && 
              tokens[*pos] != ")" && 
              tokens[*pos] != "." &&
              !self.is_binary_op(&tokens[*pos]) {
            let arg = self.parse_atom(tokens, pos)?;
            result = Term::App(Box::new(result), Box::new(arg));
        }

        Ok(result)
    }

    fn parse_expression(&self, tokens: &[String], pos: &mut usize) -> Result<Term, String> {
        self.parse_additive(tokens, pos)
    }

    // Parse addition and subtraction (lowest precedence)
    fn parse_additive(&self, tokens: &[String], pos: &mut usize) -> Result<Term, String> {
        let mut left = self.parse_multiplicative(tokens, pos)?;

        while *pos < tokens.len() {
            let op = match tokens[*pos].as_str() {
                "+" => BinOpKind::Add,
                "-" => BinOpKind::Sub,
                _ => break,
            };
            *pos += 1;
            let right = self.parse_multiplicative(tokens, pos)?;
            left = Term::BinOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    // Parse multiplication (higher precedence)
    fn parse_multiplicative(&self, tokens: &[String], pos: &mut usize) -> Result<Term, String> {
        let mut left = self.parse_term(tokens, pos)?;

        while *pos < tokens.len() && tokens[*pos] == "*" {
            *pos += 1; // consume '*'
            let right = self.parse_term(tokens, pos)?;
            left = Term::BinOp(Box::new(left), BinOpKind::Mul, Box::new(right));
        }

        Ok(left)
    }

    fn parse_atom(&self, tokens: &[String], pos: &mut usize) -> Result<Term, String> {
        if *pos >= tokens.len() {
            return Err("Unexpected end of input".to_string());
        }

        match tokens[*pos].as_str() {
            "(" => self.parse_term(tokens, pos),
            "\\" => self.parse_term(tokens, pos),
            token if token.chars().all(|c| c.is_ascii_digit()) => {
                *pos += 1;
                Ok(Term::Num(token.parse().map_err(|_| "Invalid number")?))
            }
            var => {
                *pos += 1;
                Ok(Term::Var(var.to_string()))
            }
        }
    }

    fn is_binary_op(&self, token: &str) -> bool {
        matches!(token, "+" | "-" | "*")
    }

    fn beta_reduce(&self, term: &Term) -> Term {
        match term {
            Term::Var(_) | Term::Num(_) => term.clone(),
            Term::Abs(var, body) => {
                Term::Abs(var.clone(), Box::new(self.beta_reduce(body)))
            }
            Term::App(func, arg) => {
                let func_reduced = self.beta_reduce(func);
                let arg_reduced = self.beta_reduce(arg);
                
                match func_reduced {
                    Term::Abs(var, body) => {
                        // Beta reduction: (λx.M) N → M[x := N]
                        let substituted = self.substitute(&body, &var, &arg_reduced);
                        self.beta_reduce(&substituted)
                    }
                    _ => Term::App(Box::new(func_reduced), Box::new(arg_reduced))
                }
            }
            Term::BinOp(left, op, right) => {
                let left_reduced = self.beta_reduce(left);
                let right_reduced = self.beta_reduce(right);
                
                match (&left_reduced, &right_reduced) {
                    (Term::Num(a), Term::Num(b)) => {
                        let result = match op {
                            BinOpKind::Add => a + b,
                            BinOpKind::Sub => a - b,
                            BinOpKind::Mul => a * b,
                        };
                        Term::Num(result)
                    }
                    _ => Term::BinOp(Box::new(left_reduced), op.clone(), Box::new(right_reduced))
                }
            }
        }
    }

    fn subst_helper(&self, term: &Term, var: &str, replacement: &Term, bound: &HashSet<String>) -> Term {
        match term {
            Term::Var(name) => {
                if name == var && !bound.contains(name) {
                    replacement.clone()
                } else {
                    term.clone()
                }
            }
            Term::Num(_) => term.clone(),
            Term::Abs(param, body) => {
                if param == var {
                    // Variable is shadowed, no substitution
                    term.clone()
                } else if self.free_variables(replacement).contains(param) {
                    // Alpha conversion needed to avoid capture
                    let new_param = self.fresh_var(param, &self.free_variables(replacement));
                    let renamed_body = self.substitute(body, param, &Term::Var(new_param.clone()));
                    let mut new_bound = bound.clone();
                    new_bound.insert(new_param.clone());
                    Term::Abs(new_param, Box::new(self.subst_helper(&renamed_body, var, replacement, &new_bound)))
                } else {
                    let mut new_bound = bound.clone();
                    new_bound.insert(param.clone());
                    Term::Abs(param.clone(), Box::new(self.subst_helper(body, var, replacement, &new_bound)))
                }
            }
            Term::App(func, arg) => {
                Term::App(
                    Box::new(self.subst_helper(func, var, replacement, bound)),
                    Box::new(self.subst_helper(arg, var, replacement, bound))
                )
            }
            Term::BinOp(left, op, right) => {
                Term::BinOp(
                    Box::new(self.subst_helper(left, var, replacement, bound)),
                    op.clone(),
                    Box::new(self.subst_helper(right, var, replacement, bound))
                )
            }
        }
    }

    fn free_vars_helper(&self, term: &Term, bound: &HashSet<String>) -> HashSet<String> {
        match term {
            Term::Var(name) => {
                if bound.contains(name) {
                    HashSet::new()
                } else {
                    let mut set = HashSet::new();
                    set.insert(name.clone());
                    set
                }
            }
            Term::Num(_) => HashSet::new(),
            Term::Abs(var, body) => {
                let mut new_bound = bound.clone();
                new_bound.insert(var.clone());
                self.free_vars_helper(body, &new_bound)
            }
            Term::App(func, arg) => {
                let mut free_vars = self.free_vars_helper(func, bound);
                free_vars.extend(self.free_vars_helper(arg, bound));
                free_vars
            }
            Term::BinOp(left, _, right) => {
                let mut free_vars = self.free_vars_helper(left, bound);
                free_vars.extend(self.free_vars_helper(right, bound));
                free_vars
            }
        }
    }

    fn alpha_equiv_helper(&self, term1: &Term, term2: &Term, 
                         map1: &mut HashMap<String, String>,
                         map2: &mut HashMap<String, String>) -> bool {
        match (term1, term2) {
            (Term::Var(v1), Term::Var(v2)) => {
                match (map1.get(v1), map2.get(v2)) {
                    (Some(mapped1), Some(mapped2)) => mapped1 == mapped2,
                    (None, None) => v1 == v2,
                    _ => false,
                }
            }
            (Term::Num(n1), Term::Num(n2)) => n1 == n2,
            (Term::Abs(var1, body1), Term::Abs(var2, body2)) => {
                let fresh = self.fresh_var("_tmp", &HashSet::new());
                map1.insert(var1.clone(), fresh.clone());
                map2.insert(var2.clone(), fresh);
                let result = self.alpha_equiv_helper(body1, body2, map1, map2);
                map1.remove(var1);
                map2.remove(var2);
                result
            }
            (Term::App(f1, a1), Term::App(f2, a2)) => {
                self.alpha_equiv_helper(f1, f2, map1, map2) &&
                self.alpha_equiv_helper(a1, a2, map1, map2)
            }
            (Term::BinOp(l1, op1, r1), Term::BinOp(l2, op2, r2)) => {
                op1 == op2 &&
                self.alpha_equiv_helper(l1, l2, map1, map2) &&
                self.alpha_equiv_helper(r1, r2, map1, map2)
            }
            _ => false,
        }
    }

    fn fresh_var(&self, base: &str, avoid: &HashSet<String>) -> String {
        let mut counter = 0;
        loop {
            let candidate = if counter == 0 {
                format!("{}'", base)
            } else {
                format!("{}{}", base, counter)
            };
            if !avoid.contains(&candidate) {
                return candidate;
            }
            counter += 1;
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let solver = LambdaCalculusSolver::new();
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    // Read number of expressions
    let n: usize = lines.next()
        .ok_or("No input provided")?? 
        .trim()
        .parse()?;

    for _ in 1..=n {
        if let Some(line) = lines.next() {
            let expression = line?;

            match solver.parse(&expression) {
                Ok(term) => {
                    let result = solver.evaluate(&term);
                    if let Term::Num(value) = result {
                        println!("{}", value);
                    } else {
                        println!("{}", result);
                    }
                }
                Err(_) => {
                    println!("Error");
                }
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_arithmetic() {
        let solver = LambdaCalculusSolver::new();
        let result = solver.parse("(\\x.x+3) 7").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(10));
    }

    #[test]
    fn test_parse_multiplication() {
        let solver = LambdaCalculusSolver::new();
        let result = solver.parse("(\\x.2*x) 4").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(8));
    }

    #[test]
    fn test_parse_subtraction() {
        let solver = LambdaCalculusSolver::new();
        let result = solver.parse("(\\x.x-2) 9").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(7));
    }

    #[test]
    fn test_parse_complex_expression() {
        let solver = LambdaCalculusSolver::new();
        let result = solver.parse("(\\x.x*x) 6").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(36));
    }

    #[test]
    fn test_operator_precedence() {
        let solver = LambdaCalculusSolver::new();
        // Test (y+3)*(y-1)+y*y with y=2
        // Should be: (2+3)*(2-1)+2*2 = 5*1+4 = 9
        let result = solver.parse("(\\y.(y+3)*(y-1)+y*y) 2").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(9));
    }

    #[test]
    fn test_precedence_simple() {
        let solver = LambdaCalculusSolver::new();
        // 2+3*4 should be 2+(3*4) = 14, not (2+3)*4 = 20
        let result = solver.parse("(\\x.x+3*4) 2").unwrap();
        let evaluated = solver.evaluate(&result);
        assert_eq!(evaluated, Term::Num(14));
    }
}