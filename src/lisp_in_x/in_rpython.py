import sys


class Object(object):
    _immutable_ = True

    def type(self):
        raise "No Type defined"

    def to_string(self):
        return "<Object>"

    def __str__(self):
        return self.to_string()

    def __repr__(self):
        return self.to_string()


class Type(Object):
    _immutable_ = True
    def __init__(self, type_name):
        self._type_name = type_name

    def to_string(self):
        return "<Type " + self._type_name + ">"


class Integer(Object):
    _immutable_ = True
    _type = Type("Integer")

    def __init__(self, int_val):
        self._int_val = int_val

    def to_string(self):
        return str(self._int_val)

    def type(self):
        return self._type

    def int_val(self):
        return self._int_val


class String(Object):
    _immutable_ = True
    _type = Type("String")

    def __init__(self, str_val):
        self._str_val = str_val

    def to_string(self):
        return '"%s"' % self._str_val

    def type(self):
        return self._type

class SymbolRegistry(object):
    def __init__(self):
        self._registry = {}

    def intern(self, str_val):
        sym = self._registry.get(str_val, None)

        if sym is None:
            sym = Symbol(str_val)
            self._registry[str_val] = sym

        return sym

symbol_registry = SymbolRegistry()

class Symbol(Object):
    _immutable_ = True
    _type = Type("Symbol")

    def __init__(self, str_val):
        self._str_val = str_val

    def to_string(self):
        return self._str_val

    def type(self):
        return self._type

    @staticmethod
    def intern(str_val):
        return symbol_registry.intern(str_val)


class Nil(Object):
    _immutable_ = True
    _type = Type("Nil")

    def __init__(self):
        pass

    def to_string(self):
        return "nil"

    def type(self):
        return self._type

nil = Nil()


class Boolean(Object):
    _immutable_ = True
    _type = Type("Boolean")

    def __init__(self, is_true):
        self._is_true = is_true

    def to_string(self):
        return "true" if self._is_true else "false"

    def type(self):
        return self._type

true = Boolean(True)
false = Boolean(False)


class Cons(Object):
    _immutable_ = True
    _type = Type("Cons")

    def __init__(self, car, cdr=nil):
        self._car = car
        self._cdr = cdr

    def to_string(self):
        acc = ["("]
        c = self
        while True:
            if c is nil:
                acc.append(")")
                break
            elif c.cdr() is nil:
                acc.append("%s)" % c.car().to_string())
                break
            elif isinstance(c.cdr(), Cons):
                acc.append("%s " % c.car().to_string())
                c = c.cdr()
                continue
            else:
                acc.append("%s . %s)" % (c.car().to_string(), c.cdr().to_string()))
                break

        return "".join(acc)

    def type(self):
        return self._type

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr

    @staticmethod
    def from_list(lst):
        acc = nil

        for itm in reversed(lst):
            acc = Cons(itm, acc)

        return acc

class Fn(Object):
    _type = Type("Fn")
    def __init__(self):
        self._str_name = "Fn"

    def invoke(self, args, stack):
        return nil, stack

    def to_string(self):
        return self._str_name

global_fns = {}


def defn(name):
    def inner(f):
        f = f()
        f._str_name = name
        global_fns[Symbol.intern(name)] = f
        return f
    return inner


@defn("println")
class Println(Fn):
    def invoke(self, args, stack):
        while args is not nil:
            itm = args.car()
            if isinstance(itm, String):
                sys.stdout.write(itm._str_val)
            else:
                sys.stdout.write(itm.to_string())
            args = args.cdr()

        sys.stdout.write("\n")
        return nil, stack


@defn("load-file")
class LoadFile(Fn):
    def invoke(self, args, stack):
        rdr = PushbackReader(FileReader(args.car()._str_val))
        forms = read_all(rdr)
        return nil, stack.push(EvalExpr(nil, forms))


@defn("<")
class LessThan(Fn):
    def invoke(self, args, stack):
        return true if args.car()._int_val < args.cdr().car()._int_val else false, stack


@defn(">")
class GreaterThan(Fn):
    def invoke(self, args, stack):
        return true if args.car()._int_val > args.cdr().car()._int_val else false, stack


@defn("<=")
class LessThanOrEqual(Fn):
    def invoke(self, args, stack):
        return true if args.car()._int_val <= args.cdr().car()._int_val else false, stack


@defn(">=")
class GreaterThanOrEqual(Fn):
    def invoke(self, args, stack):
        return true if args.car()._int_val >= args.cdr().car()._int_val else false, stack


@defn("=")
class Equal(Fn):
    def invoke(self, args, stack):
        a = args.car()
        b = args.cdr().car()

        if isinstance(a, Integer) and isinstance(b, Integer):
            return true if a._int_val == b._int_val else false, stack
        else:
            return true if a is b else false, stack


@defn("car")
class Car(Fn):
    def invoke(self, args, stack):
        return args.car().car(), stack


@defn("cdr")
class Cdr(Fn):
    def invoke(self, args, stack):
        return args.car().cdr(), stack


@defn("cons")
class ConsFn(Fn):
    def invoke(self, args, stack):
        return Cons(args.car(), args.cdr().car()), stack

@defn("nil?")
class NilQ(Fn):
    def invoke(self, args, stack):
        return true if args.car() is nil else false, stack

@defn("cons?")
class ConsQ(Fn):
    def invoke(self, args, stack):
        return true if isinstance(args.car(), Cons) else false, stack

@defn("symbol?")
class SymbolQ(Fn):
    def invoke(self, args, stack):
        return true if isinstance(args.car(), Symbol) else false, stack

@defn("inc")
class Inc(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() + 1), stack

@defn("dec")
class Inc(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() - 1), stack

@defn("+")
class Add(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() + args.cdr().car().int_val()), stack

@defn("-")
class Sub(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() - args.cdr().car().int_val()), stack

@defn("*")
class Mul(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() * args.cdr().car().int_val()), stack

@defn("/")
class Div(Fn):
    def invoke(self, args, stack):
        return Integer(args.car().int_val() / args.cdr().car().int_val()), stack

@defn("vararg")
class VarArg(Fn):
    def invoke(self, args, stack):
        return VarArgLambda(args.car()), stack

@defn("apply")
class Apply(Fn):
    def invoke(self, args, stack):
        fn = args.car()
        args = args.cdr().car()
        return fn.invoke(args, stack)

@defn("die")
class Die(Fn):
    def invoke(self, args, stack):
        print(args)
        assert False

@defn("read-file")
class ReadFile(Fn):
    def invoke(self, args, stack):
        rdr = PushbackReader(FileReader(args.car()._str_val))
        return read_all(rdr), stack

class VarArgLambda(Fn):
    _str_name = "VarArgLambda"
    def __init__(self, fn):
        self._fn = fn

    def invoke(self, args, stack):
        return self._fn.invoke(Cons(args), stack)



# Reader Begins

class Reader(object):
    def read(self):
        return -1


class FileReader(Reader):
    def __init__(self, file_name):
        self._file = open(file_name, "rb")

    def read(self):
        ch = self._file.read(1)
        if len(ch) == 0:
            raise EOFError()
        return ord(ch[0])


class PushbackReader(Reader):
    def __init__(self, inner):
        self._inner = inner
        self._has_unread = False
        self._unread_char = 0

    def read(self):
        if self._has_unread:
            self._has_unread = False
            return self._unread_char
        else:
            return self._inner.read()

    def unread(self, ch):
        assert not self._has_unread
        self._has_unread = True
        self._unread_char = ch


def list_reader(terminator):
    def list_reader_inner(rdr):
        ch = rdr.read()

        while ch in whitespace:
            ch = rdr.read()

        acc = []
        while True:
            if ch == terminator:
                return Cons.from_list(acc)

            rdr.unread(ch)

            acc.append(read(rdr))

            ch = rdr.read()


    return list_reader_inner

def string_reader(rdr):
    acc = []
    ch = rdr.read()
    while ch != ord("\""):
        acc.append(chr(ch))
        ch = rdr.read()

    return String("".join(acc))

def comment_reader(rdr):
    ch = rdr.read()
    while ch != ord("\n"):
        ch = rdr.read()

    return None

quote_sym = Symbol.intern("quote")
do_sym = Symbol.intern("do")
def_sym = Symbol.intern("def")
if_sym = Symbol.intern("if")
fn_sym = Symbol.intern("fn")
cond_sym = Symbol.intern("cond")
resolve_sym = Symbol.intern("resolve")
let_sym = Symbol.intern("let")


def quote_reader(rdr):
    return Cons.from_list([quote_sym, read(rdr)])


def str_to_ints(str):
    return list(map(ord, str))

whitespace = list(map(ord, "\n\t \r,"))

macros = {ord("("): list_reader(ord(")")),
          ord("["): list_reader(ord("]")),
          ord("\""): string_reader,
          ord(";"): comment_reader,
          ord("'"): quote_reader}

alphanums = str_to_ints("1234567890abcdefghijklmnopqrstuvwxyz_!-+*/<>=?")

nums = str_to_ints("1234567890")

def symbol_reader(rdr, start):
    acc = []
    ch = start

    while ch in alphanums:
        acc.append(chr(ch))
        ch = rdr.read()

    rdr.unread(ch)

    return interpret_symbol("".join(acc))

direct_mappings = {"true": true,
                   "false": false,
                   "nil": nil}

def interpret_symbol(sym):
    direct = direct_mappings.get(sym, None)
    if direct is not None:
        return direct

    if ord(sym[0]) in nums or (sym[0] == "-" and len(sym) > 1 and ord(sym[1]) in nums):
        return Integer(int(sym))
    return Symbol.intern(sym)


def read(rdr):

    while True:
        ch = rdr.read()
        while ch in whitespace:
            ch = rdr.read()

        macro = macros.get(ch, None)
        if macro is not None:
            result = macro(rdr)

            if result is None:
                continue

            return result

        return symbol_reader(rdr, ch)

def read_all(rdr):
    acc = [do_sym]
    while True:
        try:
            acc.append(read(rdr))
        except EOFError:
            return Cons.from_list(acc)
# End of Reader Code

# Start of Interpreter

class Stack(object):
    _immutable_ = True
    def __init__(self, k=None, prev=None):
        self._k = k
        self._prev = prev

    def push(self, k):
        return Stack(k, self)

    def pop(self):
        return self._k, self._prev

    def has_more(self):
        return self._k is not None

tos = Stack()

class Continuation(object):
    _immutable_ = True
    def call_continuation(self, val, stack):
        return val, stack

class EvalExpr(Continuation):
    _immutable_ = True

    def __init__(self, env, expr):
        self._env = env
        self._expr = expr

    def call_continuation(self, val, stack):
        return eval_one(self._env, self._expr, stack)

class EvalApply(Continuation):
    _immutable_ = True

    def __init__(self, env, exprs, expr_count=1):
        self._env = env
        self._exprs = exprs
        self._expr_count = expr_count

    def call_continuation(self, val, stack):
        old_stack = stack
        if self._exprs is nil:
            if self._expr_count == 1:
                f = val
                args = nil
            else:
                args = Cons(val)

                for x in range(self._expr_count - 2):
                    k, stack = stack.pop()
                    args = Cons(k.val(), args)
                k, stack = stack.pop()
                f = k.val()
            return f.invoke(args, stack)

        else:
            stack = stack.push(Val(val)) \
                         .push(EvalApply(self._env, self._exprs.cdr(), self._expr_count + 1)) \
                         .push(EvalExpr(self._env, self._exprs.car()))
            return nil, stack

class Val(Continuation):
    _immutable_ = True

    def __init__(self, val):
        self._val = val

    def val(self):
        return self._val

class DoContinuation(Continuation):
    _immutable_ = True

    def __init__(self, env, exprs):
        self._env = env
        self._args = exprs

    def call_continuation(self, val, stack):
        if self._args.cdr() is nil:
            return nil, stack.push(EvalExpr(self._env, self._args.car()))
        else:
            return nil, stack.push(DoContinuation(self._env, self._args.cdr())) \
                             .push(EvalExpr(self._env, self._args.car()))

class DefContinuation(Continuation):
    _immutable_ = True

    def __init__(self, sym):
        self._sym = sym

    def call_continuation(self, val, stack):
        global_registry.def_global(self._sym, val)
        return val, stack

class IfContinuation(Continuation):
    _immutable_ = True

    def __init__(self, env, then_expr, else_expr):
        self._env = env
        self._then_expr = then_expr
        self._else_expr = else_expr

    def call_continuation(self, val, stack):
        if val is nil or val is false:
            return nil, stack.push(EvalExpr(self._env, self._else_expr))
        else:
            return nil, stack.push(EvalExpr(self._env, self._then_expr))

class CondContinuation(Continuation):
    def __init__(self, env, exprs):
        self._env = env
        self._exprs = exprs

    def call_continuation(self, val, stack):
        if val is not nil and val is not false:
            return nil, stack.push(EvalExpr(self._env, self._exprs.car()))
        elif self._exprs.cdr() is nil:
            return nil, stack
        else:
            return nil, stack.push(CondContinuation(self._env, self._exprs.cdr().cdr())) \
                             .push(EvalExpr(self._env, self._exprs.cdr().car()))

class LetContinuation(Continuation):
    def __init__(self, env, sym, bind, body):
        self._sym = sym
        self._env = env
        self._bind = bind
        self._body = body

    def call_continuation(self, val, stack):
        new_env = Cons(Cons(self._sym, val), self._env)
        if self._bind is nil:
            return nil, stack.push(DoContinuation(new_env, self._body))
        else:
            return nil, stack.push(LetContinuation(new_env, self._bind.car(), self._bind.cdr().cdr(), self._body)) \
                             .push(EvalExpr(new_env, self._bind.cdr().car()))


class ResolveContinuation(Continuation):
    def __init__(self):
        pass

    def call_continuation(self, val, stack):
        return global_registry.get_global(val), stack


class Lambda(Fn):
    _immutable_ = True
    def __init__(self, env, arg_list, body):
        self._env = env
        self._arg_list = arg_list
        self._body = body

    def to_string(self):
        return "Lambda"

    def invoke(self, args, stack):
        new_env = self._env
        arg_list = self._arg_list
        while args is not nil:
            new_env = Cons(Cons(arg_list.car(), args.car()), new_env)
            arg_list = arg_list.cdr()
            args = args.cdr()

        return nil, stack.push(EvalExpr(new_env, self._body))

def eval_sexpr(env, sym, args, stack):
    if sym is if_sym:
        return nil, stack.push(IfContinuation(env, args.cdr().car(), args.cdr().cdr().car())) \
                         .push(EvalExpr(env, args.car()))
    elif sym is do_sym:
        return nil, stack.push(DoContinuation(env, args))
    elif sym is def_sym:
        return nil, stack.push(DefContinuation(args.car())) \
                         .push(EvalExpr(env, args.cdr().car()))
    elif sym is quote_sym:
        return args.car(), stack
    elif sym is cond_sym:
        return nil, stack.push(CondContinuation(env, args.cdr())) \
                         .push(EvalExpr(env, args.car()))
    elif sym is resolve_sym:
        return nil, stack.push(ResolveContinuation()) \
                         .push(EvalExpr(env, args.car()))
    elif sym is let_sym:
        binds = args.car()
        body = args.cdr()
        return nil, stack.push(LetContinuation(env, binds.car(), binds.cdr().cdr(), body)) \
                         .push(EvalExpr(env, binds.cdr().car()))
    elif sym is fn_sym:
        arg_list = args.car()
        body = Cons(do_sym, args.cdr())
        return Lambda(env, arg_list, body), stack

    return nil, stack.push(EvalApply(env, args)) \
                     .push(EvalExpr(env, sym))

def eval_one(env, expr, stack):
    if isinstance(expr, Cons):
        if isinstance(expr.car(), Symbol):
            return eval_sexpr(env, expr.car(), expr.cdr(), stack)
        else:
            return nil, stack.push(EvalApply(env, expr.cdr())) \
                             .push(EvalExpr(env, expr.car()))
    elif isinstance(expr, Symbol):
        return lookup(env, expr), stack
    else:
        return expr, stack

class Globals(object):
    _immutable_fields_ = ["_globals"]
    def __init__(self):
        self._globals = {}

    def def_global(self, k, v):
        self._globals[k] = v

    def get_global(self, k):
        return self._globals[k]

    def clear(self):
        self._globals.clear()

global_registry = Globals()


def reset_globals():
    global_registry.clear()
    for k, v in global_fns.items():
        global_registry.def_global(k, v)

reset_globals()

def lookup(env, k):
    e = env
    while e is not nil:
        if e.car().car() is k:
            return e.car().cdr()

        e = e.cdr()

    return global_registry.get_global(k)

def eval_all(expr):
    env = nil
    stack = tos
    val, stack = eval_one(env, expr, stack)

    while stack.has_more():
        k, stack = stack.pop()
        val, stack = k.call_continuation(val, stack)

    return val


# Entry Point code

def run(filename):
    rdr = PushbackReader(FileReader("src/lisp_in_x/tests.clj"))
    forms = read_all(rdr)
    result = eval_all(forms)


    return result


def entry_point(argv):
    return run(argv[1])

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    entry_point("src/lisp_in_x/tests.clj")











