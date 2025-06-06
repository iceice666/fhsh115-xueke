#!/usr/bin/env python3
import sys
from typing import Union, Tuple, List, cast, Any

sys.setrecursionlimit(10000)


def tokenize(s: str) -> List[str]:
    tokens: List[str] = []
    i = 0
    while i < len(s):
        if s[i].isspace():
            i += 1
        elif s[i] in "()\\.*+-":
            tokens.append(s[i])
            i += 1
        elif s[i].isdigit():
            num = ""
            while i < len(s) and s[i].isdigit():
                num += s[i]
                i += 1
            tokens.append(num)
        elif s[i].isalpha() or s[i] == "_":
            var = ""
            while i < len(s) and (s[i].isalnum() or s[i] == "_"):
                var += s[i]
                i += 1
            tokens.append(var)
        else:
            i += 1
    return tokens


ExprType = Union[
    Tuple[str, int],  # ("num", value)
    Tuple[str, str],  # ("var", name)
    Tuple[str, str, 'ExprType'],  # ("lambda", param, body)
    Tuple[str, 'ExprType', 'ExprType'],  # ("app", func, arg)
    Tuple[str, str, 'ExprType', 'ExprType']  # ("binop", op, left, right)
]

def parse_expr(tokens: List[str], pos: List[int]) -> ExprType:
    return parse_add_sub(tokens, pos)


def parse_add_sub(tokens: List[str], pos: List[int]) -> ExprType:
    left = parse_mul(tokens, pos)
    while pos[0] < len(tokens) and tokens[pos[0]] in ["+", "-"]:
        op = tokens[pos[0]]
        pos[0] += 1
        right = parse_mul(tokens, pos)
        left = ("binop", op, left, right)
    return left


def parse_mul(tokens: List[str], pos: List[int]) -> ExprType:
    left = parse_app(tokens, pos)
    while pos[0] < len(tokens) and tokens[pos[0]] == "*":
        pos[0] += 1
        right = parse_app(tokens, pos)
        left = ("binop", "*", left, right)
    return left


def parse_app(tokens: List[str], pos: List[int]) -> ExprType:
    left = parse_atom(tokens, pos)
    while pos[0] < len(tokens) and tokens[pos[0]] not in [")", "+", "-", "*", "."]:
        right = parse_atom(tokens, pos)
        left = ("app", left, right)
    return left


def parse_atom(tokens: List[str], pos: List[int]) -> ExprType:
    if pos[0] >= len(tokens):
        raise ValueError("Unexpected end")

    token = tokens[pos[0]]

    if token == "(":
        pos[0] += 1
        expr = parse_expr(tokens, pos)
        if pos[0] >= len(tokens) or tokens[pos[0]] != ")":
            raise ValueError("Missing )")
        pos[0] += 1
        return expr
    elif token == "\\":
        pos[0] += 1
        var = tokens[pos[0]]
        pos[0] += 1
        if tokens[pos[0]] != ".":
            raise ValueError("Missing .")
        pos[0] += 1
        body = parse_expr(tokens, pos)
        return ("lambda", var, body)
    elif token.isdigit():
        pos[0] += 1
        return ("num", int(token))
    else:
        pos[0] += 1
        return ("var", token)


def substitute(expr: ExprType, var: str, value: ExprType) -> ExprType:
    if expr[0] == "num":
        return expr
    elif expr[0] == "var":
        if expr[1] == var:
            return value
        return expr
    elif expr[0] == "lambda":
        # expr is ("lambda", param, body)
        expr_any = cast(Any, expr)
        param = expr_any[1]
        body = expr_any[2]
        if param == var:
            return expr
        return ("lambda", param, substitute(body, var, value))
    elif expr[0] == "app":
        # expr is ("app", func, arg)
        expr_any = cast(Any, expr)
        func = expr_any[1]
        arg = expr_any[2]
        return ("app", substitute(func, var, value), substitute(arg, var, value))
    elif expr[0] == "binop":
        # expr is ("binop", op, left, right)
        expr_any = cast(Any, expr)
        op = expr_any[1]
        left = expr_any[2]
        right = expr_any[3]
        return (
            "binop",
            op,
            substitute(left, var, value),
            substitute(right, var, value),
        )
    return expr


def evaluate(expr: ExprType) -> ExprType:
    if expr[0] == "num":
        return expr
    elif expr[0] == "var":
        return expr
    elif expr[0] == "lambda":
        # expr is ("lambda", param, body)
        expr_any = cast(Any, expr)
        param = expr_any[1]
        body = expr_any[2]
        return ("lambda", param, evaluate(body))
    elif expr[0] == "app":
        # expr is ("app", func, arg)
        expr_any = cast(Any, expr)
        func_expr = expr_any[1]
        arg_expr = expr_any[2]
        func = evaluate(func_expr)
        arg = evaluate(arg_expr)
        if func[0] == "lambda":
            func_any = cast(Any, func)
            param = func_any[1]
            body = func_any[2]
            substituted = substitute(body, param, arg)
            return evaluate(substituted)
        return ("app", func, arg)
    elif expr[0] == "binop":
        # expr is ("binop", op, left, right)
        expr_any = cast(Any, expr)
        op = expr_any[1]
        left_expr = expr_any[2]
        right_expr = expr_any[3]
        left = evaluate(left_expr)
        right = evaluate(right_expr)
        if left[0] == "num" and right[0] == "num":
            left_any = cast(Any, left)
            right_any = cast(Any, right)
            left_val = left_any[1]
            right_val = right_any[1]
            if op == "+":
                return ("num", left_val + right_val)
            elif op == "-":
                return ("num", left_val - right_val)
            elif op == "*":
                return ("num", left_val * right_val)
        return ("binop", op, left, right)
    return expr


def solve(line: str) -> int:
    tokens = tokenize(line)
    pos: List[int] = [0]
    expr = parse_expr(tokens, pos)
    result = evaluate(expr)
    # The result should be a "num" expression
    if result[0] == "num":
        result_any = cast(Any, result)
        return result_any[1]
    else:
        # If it's not a number, return 0 as fallback
        return 0


def main() -> None:
    n = int(input().strip())
    for _ in range(n):
        line = input().strip()
        print(solve(line))


if __name__ == "__main__":
    main()
