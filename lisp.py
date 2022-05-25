class Token:
    def __init__(self, string):
        self.value = string

    def __str__(self):
        return '<token: \'' + self.value + '\'>'

class Symbol:
    symtable = {}

    @staticmethod
    def from_str(string):
        if string in Symbol.symtable:
            return Symbol.symtable[string]
        else:
            sym = Symbol()
            sym.string = string
            Symbol.symtable[sym.string] = sym
            return sym

    def __str__(self):
        return self.string

class Nil:
    def __str__(self):
        return '()'
NIL = Nil()

class Pair:
    def __init__(self, car=None, cdr=None):
        self.car = car
        self.cdr = cdr

    def __str__(self):
        result = '('
        x = self
        while True:
            result += x.car.__str__()
            x = x.cdr
            if type(x) == Pair:
                result += ' '
            else:
                break
        return result + ')'

def list_map(lst, f):
    if type(lst) != Pair:
        return lst
    new_lst = Pair(f(lst.car), NIL)
    tail = new_lst
    lst = lst.cdr
    while type(lst) == Pair:
        tail.cdr = Pair(f(lst.car), NIL)
        tail = tail.cdr
        lst = lst.cdr
    return new_lst

class Closure:
    def __init__(self, argnames, body, env):
        self.argnames = argnames
        self.body = body
        self.env = env
        
class Env:
    def __init__(self):
        self.table = {}
        self.next = None

    def get(self, sym):
        frame = self
        while frame is not None:
            if sym in frame.table:
                return frame.table[sym]
            frame = frame.next
        raise Exception('undefined symbol \''+ sym.__str__() + '\' in ' + self.__str__())

    def set(self, sym, value):
        frame = self
        while frame is not None:
            if sym in frame.table:
                frame.table[sym] = value
                return
            frame = frame.next
        self.table[sym] = value

    def append(self, frame):
        last = self
        while last.next is not None:
            last = last.next
        last.next = frame

class Reader:
    def __init__(self, source):
        self.saved_token = None
        self.source = source
        self.pos = 0

    def get_char(self):
        if self.pos >= len(self.source):
            return None
        self.pos += 1
        return self.source[self.pos - 1]

    def char_look_ahead(self):
        if self.pos >= len(self.source):
            return None
        return self.source[self.pos]
        
    def get_new_token(self):
        c = self.get_char()
        while c is not None and c in ' \t\n\r':
            c = self.get_char()
        if c is None:
            return None
        if c in '()':
            return Token(c)
        string = c
        while not (self.char_look_ahead() is None or self.char_look_ahead() in ' \t\n\r()'):
            string += self.get_char()
        try:
            return int(string)
        except:
            pass
        try:
            return float(string)
        except:
            pass
        return Symbol.from_str(string)

    def get_token(self):
        if self.saved_token is not None:
            result = self.saved_token
            self.saved_token = None
            return result
        return self.get_new_token()

    def token_look_ahead(self):
        self.saved_token = self.get_new_token()
        return self.saved_token

    def parse(self):
        t = self.get_token()
        if t is None:
            return None
        if type(t) != Token:
            return t
        if t.value != '(':
            raise Exception('Unexpected token ' + t.value)
        acc = NIL
        tail = None
        while True:
            t = self.token_look_ahead()
            if t is None:
                raise Exception('Unexpected end of file')
            if type(t) == Token and t.value == ')':
                self.get_token()
                break;
            new_tail = Pair(self.parse(), NIL)
            if acc == NIL:
                acc = new_tail
            else:
                tail.cdr = new_tail
            tail = new_tail
        return acc

    def parse_all(self):
        exprs = []
        expr = self.parse()
        while expr is not None:
            exprs.append(expr)
            expr = self.parse()
        return exprs

    
global_env = Env()

class LispBuiltin:
    def __init__(self, f, f_type):
        self.f = f
        self.f_type = f_type
    
    def __call__(self, args, env):
        return self.f(args, env)

    def __str__(self):
        return '<built-in ' + self.f_type + ': ' + self.f.__name__ + '>'
    
def lisp_form(name):
    def inner(f):
        global_env.set(Symbol.from_str(name), LispBuiltin(f, 'special form'))
        return f
    return inner

def lisp_func(name):
    def inner(f):
        def wrapper(args, env):
            evaled_args = list_map(args, lambda a: lisp_eval(a, env))
            return f(evaled_args)
        wrapper.__name__ = f.__name__
        global_env.set(Symbol.from_str(name), LispBuiltin(wrapper, 'function'))
        return f
    return inner

@lisp_form('lambda')
def L_lambda(args, env):
    argnames = args.car
    body = args.cdr
    return Closure(argnames, body, env)

@lisp_form('q')
def L_quote(args, env):
    return args.car

@lisp_func('+')
def L_plus(args):
    acc = 0
    while type(args) == Pair:
        acc += args.car
        args = args.cdr
    return acc

@lisp_func('-')
def L_minus(args):
    acc = args.car
    args = args.cdr
    while type(args) == Pair:
        acc -= args.car
        args = args.cdr
    return acc

@lisp_func('*')
def L_mult(args):
    acc = 1
    while type(args) == Pair:
        acc *= args.car
        args = args.cdr
    return acc

@lisp_func('<')
def L_less(args):
    return args.car < args.cdr.car or NIL


SYMBOL_IF = Symbol.from_str('if')
global_env.set(SYMBOL_IF, SYMBOL_IF)

global_env.set(Symbol.from_str('nil'), NIL)

def lisp_eval(expr, env):
    while True:
        if type(expr) == Symbol:
            return env.get(expr)
        if type(expr) != Pair:
            return expr
        f = lisp_eval(expr.car, env)
        args = expr.cdr
        if f == SYMBOL_IF:
            if lisp_eval(args.car, env) != NIL:
                expr = args.cdr.car
            else:
                expr = args.cdr.cdr.car
            continue
        if type(f) == LispBuiltin:
            return f(args, env)
        if type(f) != Closure:
            raise Exception('not a function: ' + f.__str__())
        argnames = f.argnames
        new_env = Env()
        new_env.set(Symbol.from_str('self'), f)
        while type(args) == Pair and type(argnames) == Pair:
            new_env.set(argnames.car, lisp_eval(args.car, env))
            argnames = argnames.cdr
            args = args.cdr
        new_env.append(f.env)
        body = f.body
        while type(body.cdr) == Pair:
            lisp_eval(body.car, new_env)
            body = body.cdr
        # optimize tail recursion (previously: 'return lisp_eval(body.car, new_env)')
        expr = body.car
        env = new_env
        continue

import readline
import traceback
def repl():
    while True:
        try:
            string = input('> ')
        except EOFError:
            print()
            return
        except KeyboardInterrupt:
            print()
            return
        for expr in Reader(string).parse_all():
            try:
                result = lisp_eval(expr, global_env)
                print(result)
            except Exception:
                print(traceback.format_exc())

def example():
    exprs = Reader("""
    ((lambda (x) (if x (+ 1 2 3) (* 4 5 6))) ())
    ((lambda (n) (if (< n 1) 1 (* n (self (- n 1))))) 6)
    ((lambda (n acc) (if (< n 1) acc (self (- n 1) (* acc n)))) 1000 1)
    """).parse_all()
    for expr in exprs:
        print(expr)
        print(lisp_eval(expr, global_env))

example()
repl()
