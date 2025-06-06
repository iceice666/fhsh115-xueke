#!/usr/bin/env python3
"""
Lambda calculus evaluator for competitive programming.
Supports parsing and evaluating lambda expressions with arithmetic operations.
"""

import re
import sys
from typing import List, Union, Optional
from enum import Enum
from dataclasses import dataclass

# Increase recursion limit for deep lambda expressions
sys.setrecursionlimit(20000)


class TermType(Enum):
    VAR = "variable"
    ABS = "abstraction"
    APP = "application"
    NUM = "number"
    BINOP = "binary_operation"


class BinOpKind(Enum):
    ADD = "+"
    SUB = "-"
    MUL = "*"


@dataclass
class Term:
    """Represents a lambda calculus term."""
    term_type: TermType
    var_name: Optional[str] = None
    left: Optional['Term'] = None
    right: Optional['Term'] = None
    body: Optional['Term'] = None
    num_val: Optional[int] = None
    op: Optional[BinOpKind] = None


class LambdaCalculusSolver:
    """Lambda calculus parser and evaluator."""
    
    def __init__(self):
        pass
    
    def tokenize(self, input_str: str) -> List[str]:
        """Tokenize the input string into a list of tokens."""
        tokens = []
        i = 0
        n = len(input_str)
        
        while i < n:
            c = input_str[i]
            
            # Skip whitespace
            if c.isspace():
                i += 1
                continue
            
            # Single character tokens
            if c in "()\\+*-.":
                tokens.append(c)
                i += 1
            # Numbers
            elif c.isdigit():
                num = ""
                while i < n and input_str[i].isdigit():
                    num += input_str[i]
                    i += 1
                tokens.append(num)
            # Variables/identifiers
            elif c.isalpha() or c == '_':
                var = ""
                while i < n and (input_str[i].isalnum() or input_str[i] == '_'):
                    var += input_str[i]
                    i += 1
                tokens.append(var)
            else:
                i += 1  # Skip unknown characters
        
        return tokens
    
    def is_binop(self, token: str) -> bool:
        """Check if token is a binary operator."""
        return token in ["+", "-", "*"]
    
    def parse_atom(self, tokens: List[str], pos: List[int]) -> Term:
        """Parse an atomic expression."""
        if pos[0] >= len(tokens):
            raise ValueError("Unexpected end of input")
        
        token = tokens[pos[0]]
        
        if token == "(":
            pos[0] += 1  # skip '('
            term = self.parse_expr(tokens, pos)
            if pos[0] >= len(tokens) or tokens[pos[0]] != ")":
                raise ValueError("Expected closing parenthesis")
            pos[0] += 1  # skip ')'
            return term
        elif token == "\\":
            pos[0] += 1  # skip '\'
            if pos[0] >= len(tokens):
                raise ValueError("Expected variable after \\")
            var = tokens[pos[0]]
            pos[0] += 1
            if pos[0] >= len(tokens) or tokens[pos[0]] != ".":
                raise ValueError("Expected '.' after variable")
            pos[0] += 1  # skip '.'
            body = self.parse_expr(tokens, pos)
            return Term(TermType.ABS, var_name=var, body=body)
        elif token.isdigit():
            pos[0] += 1
            return Term(TermType.NUM, num_val=int(token))
        else:
            # Variable
            pos[0] += 1
            return Term(TermType.VAR, var_name=token)
    
    def parse_term(self, tokens: List[str], pos: List[int]) -> Term:
        """Parse a term with function applications."""
        term = self.parse_atom(tokens, pos)
        
        # Handle function applications (left-associative)
        while (pos[0] < len(tokens) and 
               tokens[pos[0]] != ")" and 
               tokens[pos[0]] != "." and
               not self.is_binop(tokens[pos[0]])):
            arg = self.parse_atom(tokens, pos)
            term = Term(TermType.APP, left=term, right=arg)
        
        return term
    
    def parse_mul(self, tokens: List[str], pos: List[int]) -> Term:
        """Parse multiplication (higher precedence)."""
        left = self.parse_term(tokens, pos)
        
        while pos[0] < len(tokens) and tokens[pos[0]] == "*":
            pos[0] += 1  # consume '*'
            right = self.parse_term(tokens, pos)
            left = Term(TermType.BINOP, left=left, right=right, op=BinOpKind.MUL)
        
        return left
    
    def parse_add(self, tokens: List[str], pos: List[int]) -> Term:
        """Parse addition and subtraction (lower precedence)."""
        left = self.parse_mul(tokens, pos)
        
        while pos[0] < len(tokens):
            token = tokens[pos[0]]
            if token == "+":
                op = BinOpKind.ADD
            elif token == "-":
                op = BinOpKind.SUB
            else:
                break
            
            pos[0] += 1
            right = self.parse_mul(tokens, pos)
            left = Term(TermType.BINOP, left=left, right=right, op=op)
        
        return left
    
    def parse_expr(self, tokens: List[str], pos: List[int]) -> Term:
        """Parse a complete expression."""
        return self.parse_add(tokens, pos)
    
    def parse(self, input_str: str) -> Term:
        """Parse a lambda expression from string."""
        tokens = self.tokenize(input_str)
        pos = [0]  # Use list to make it mutable in nested calls
        return self.parse_expr(tokens, pos)
    
    def substitute(self, term: Term, var: str, replacement: Term) -> Term:
        """Substitute variable with replacement term."""
        if term.term_type == TermType.VAR:
            if term.var_name == var:
                return replacement
            else:
                return term
        elif term.term_type == TermType.NUM:
            return term
        elif term.term_type == TermType.ABS:
            if term.var_name == var:
                # Variable is shadowed
                return term
            else:
                new_body = self.substitute(term.body, var, replacement)
                return Term(TermType.ABS, var_name=term.var_name, body=new_body)
        elif term.term_type == TermType.APP:
            new_left = self.substitute(term.left, var, replacement)
            new_right = self.substitute(term.right, var, replacement)
            return Term(TermType.APP, left=new_left, right=new_right)
        elif term.term_type == TermType.BINOP:
            new_left = self.substitute(term.left, var, replacement)
            new_right = self.substitute(term.right, var, replacement)
            return Term(TermType.BINOP, left=new_left, right=new_right, op=term.op)
        
        return term
    
    def evaluate(self, term: Term) -> Term:
        """Evaluate a lambda expression using beta reduction."""
        if term.term_type in [TermType.VAR, TermType.NUM]:
            return term
        elif term.term_type == TermType.ABS:
            new_body = self.evaluate(term.body)
            return Term(TermType.ABS, var_name=term.var_name, body=new_body)
        elif term.term_type == TermType.APP:
            func = self.evaluate(term.left)
            arg = self.evaluate(term.right)
            
            if func.term_type == TermType.ABS:
                # Beta reduction: (λx.M) N → M[x := N]
                substituted = self.substitute(func.body, func.var_name, arg)
                return self.evaluate(substituted)
            else:
                return Term(TermType.APP, left=func, right=arg)
        elif term.term_type == TermType.BINOP:
            left = self.evaluate(term.left)
            right = self.evaluate(term.right)
            
            if left.term_type == TermType.NUM and right.term_type == TermType.NUM:
                if term.op == BinOpKind.ADD:
                    result = left.num_val + right.num_val
                elif term.op == BinOpKind.SUB:
                    result = left.num_val - right.num_val
                elif term.op == BinOpKind.MUL:
                    result = left.num_val * right.num_val
                else:
                    raise ValueError(f"Unknown operator: {term.op}")
                
                return Term(TermType.NUM, num_val=result)
            else:
                return Term(TermType.BINOP, left=left, right=right, op=term.op)
        
        return term


def main():
    """Main function to read input and process lambda expressions."""
    solver = LambdaCalculusSolver()
    
    n = int(input().strip())
    
    for _ in range(n):
        line = input().strip()
        try:
            term = solver.parse(line)
            result = solver.evaluate(term)
            print(result.num_val)
        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    main()