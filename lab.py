"""6.009 Lab 9: Carlae Interpreter Part 2"""

import sys
from xmlrpc.client import Boolean
sys.setrecursionlimit(10_000)
import doctest

# NO ADDITIONAL IMPORTS!


###########################
# Carlae-related Exceptions #
###########################


class CarlaeError(Exception):
    """
    A type of exception to be raised if there is an error with a Carlae
    program.  Should never be raised directly; rather, subclasses should be
    raised.
    """

    pass


class CarlaeSyntaxError(CarlaeError):
    """
    Exception to be raised when trying to evaluate a malformed expression.
    """

    pass


class CarlaeNameError(CarlaeError):
    """
    Exception to be raised when looking up a name that has not been defined.
    """

    pass


class CarlaeEvaluationError(CarlaeError):
    """
    Exception to be raised if there is an error during evaluation other than a
    CarlaeNameError.
    """

    pass


class Environment():
    """
    Binds variables to values, can be parents or children of other Environments
    """
    def __init__(self, parent, vars=None):
        if vars: # user can pass in environment variables upon initialization
            self.vars = vars
        else:
            self.vars = dict() # empty dict otherwise
        
        self.parent = parent
    
    def get_val(self, var):
        if var in self.vars:
            return self.vars[var]
        try:
            return self.parent.get_val(var) # look in parent environment
        except:
            raise CarlaeNameError("Variable not found")
    
    def set_val(self, var, val):
        self.vars[var] = val
    
    def del_val(self, var):
        if var in self.vars:
            return self.vars.pop(var)
        raise CarlaeNameError("Variable not found")
    
    def set_bang(self, var, val):
        if var in self.vars:
            self.vars[var] = val
            return val
        try:
            return self.parent.set_bang(var, val) # look in parent environment
        except:
            raise CarlaeNameError("Variable not found")

class Function():
    """
    A callable class for a function with parameters and a body inside a given environment
    """
    def __init__(self, env, params, body):
        self.env = env
        self.params = params
        self.body = body

    def __call__(self, args):
        if len(args) != len(self.params): # if args and params don't match up
            raise CarlaeEvaluationError("Incorrect number of args passed in")

        new = Environment(self.env, {param: arg for param, arg in zip(self.params, args)})
        return evaluate(self.body, new) # handles call-ability of inputs

    def __str__(self):
        return "function object"

class Pair():
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail
    def get_head(self):
        return self.head
    def get_tail(self):
        return self.tail
    def set_tail(self, new):
        self.tail = new
    def set_head(self, new):
        self.head = new
    def __str__(self):
        return "(" + str(self.head) + ", " + str(self.tail) + ")"
    def copy(self):
        if self.tail == "nil":
            return Pair(self.head, self.tail)
        return Pair(self.head, self.tail.copy())


############################
# Tokenization and Parsing #
############################


def number_or_symbol(x):
    """
    Helper function: given a string, convert it to an integer or a float if
    possible; otherwise, return the string itself

    >>> number_or_symbol("8")
    8
    >>> number_or_symbol("-5.32")
    -5.32
    >>> number_or_symbol("1.2.3.4")
    "1.2.3.4"
    >>> number_or_symbol("x")
    "x"
    """
    try:
        return int(x)
    except ValueError:
        try:
            return float(x)
        except ValueError:
            return x


def tokenize(source):
    """
    Splits an input string into meaningful tokens (left parens, right parens,
    other whitespace-separated values).  Returns a list of strings.

    Arguments:
        source (str): a string containing the source code of a Carlae
                      expression
    """
    # remove comments
    lines = source.split("\n")
    source = ""
    for line in lines:
        try:
            source += line[:line.index("#")] + " " # preserve space between lines
        except:
            source += line + " "
    
    source = source.split() # first split by whitespace
    tokens = []
    for elt in source:
        temp = "" # temp string keeping track of non-parenthetical chars
        for char in elt: # traverse each character
            if char not in {"(", ")"}:
                temp += char
            else:
                # add completed token to tokens
                if temp:
                    tokens.append(temp)
                tokens.append(char)
                temp = "" # reset token string
        if temp: # add leftover
            tokens.append(temp)
    return tokens


def parse(tokens):
    """
    Parses a list of tokens, constructing a representation where:
        * symbols are represented as Python strings
        * numbers are represented as Python ints or floats
        * S-expressions are represented as Python lists

    Arguments:
        tokens (list): a list of strings representing tokens
    """
    # preliminary checks
    if tokens[0] != "(" and len(tokens) != 1:
        raise CarlaeSyntaxError("Invalid expression")
    if tokens.count(")") != tokens.count("("):
        raise CarlaeSyntaxError("Invalid expression")

    # helper function, recursive
    def parse_expression(index):
        if tokens[index] == ")": # should not see a closed parentheses
            raise CarlaeSyntaxError("Invalid expression")
        if tokens[index] != "(":
            return number_or_symbol(tokens[index]), index + 1
        
        out = []
        index += 1
        try:
            while tokens[index] != ")": # end of expression
                var, index = parse_expression(index) # recursive call
                out.append(var)
        except:
            raise CarlaeSyntaxError("Invalid expression")
        return out, index + 1

    return parse_expression(0)[0]


######################
# Built-in Functions #
######################
def comparisons(func_name):
    """
    Function that will return the proper comparison function
    from a function name input
    """
    if func_name == "equal":
        def equal(args):
            for i in range(len(args) - 1):
                # every consecutive arg must be equal
                if args[i] != args[i + 1]:
                    return False
            return True
        return equal

    if func_name == "dec":
        def dec(args):
            for i in range(len(args) - 1):
                # no arg can be less than or equal to the next
                if args[i] <= args[i + 1]:
                    return False
            return True
        return dec

    if func_name == "non_inc":
        def non_inc(args):
            for i in range(len(args) - 1):
                # no arg can be less than the next
                if args[i] < args[i + 1]:
                    return False
            return True
        return non_inc

    if func_name == "inc":
        def inc(args):
            for i in range(len(args) - 1):
                # no arg can be greater than or equal to the next
                if args[i] >= args[i + 1]:
                    return False
            return True
        return inc
    if func_name == "non_dec":
        def non_dec(args):
            for i in range(len(args) - 1):
                # no arg can be greater than the next
                if args[i] > args[i + 1]:
                    return False
            return True
        return non_dec

    if func_name == "not_func":
        def not_func(args):
            # input validation
            if len(args) != 1:
                raise CarlaeEvaluationError("Too many arguments for 'not' function")
            if not isinstance(args[0], Boolean):
                raise CarlaeEvaluationError("Not a not-able input")
            
            return not args[0]
        return not_func

##################
# Pair Functions #
##################

def pair(args):
    """ Creates a pair from two inputs """
    # input validation
    if len(args) != 2:
        raise CarlaeEvaluationError("Pairs need to have two arguments")
    
    return Pair(args[0], args[1])

def head(args):
    """
    Gets the head of a Pair object
    """
    # input validation
    if len(args) != 1:
        raise CarlaeEvaluationError("Only one input allowed")
    if not isinstance(args[0], Pair):
        raise CarlaeEvaluationError("Input must be a pair")
    
    return args[0].get_head()

def tail(args):
    """
    Gets the tail of a Pair object
    """
    # input validation
    if len(args) != 1:
        raise CarlaeEvaluationError("Only one input allowed")
    if not isinstance(args[0], Pair):
        raise CarlaeEvaluationError("Input must be a pair")
    
    return args[0].get_tail()

def list_func(args):
    """
    Creates a list from inputs
    """
    # base case
    if not args:
        return "nil"
    # recursive call
    return Pair(args[0], list_func(args[1:]))

def islist(args):
    """
    Checks if an input is a list
    """
    # base case
    if args[0] == "nil":
        return True
    # recursive call
    if isinstance(args[0], Pair) and islist([args[0].get_tail()]):
        return True
    
    return False # else

def length(args):
    """
    Returns the length of a list
    """
    # input validation
    if not islist(args):
        raise CarlaeEvaluationError("Input must be a list")
    # base case
    if args[0] == "nil":
        return 0
    # recursive call
    return 1 + length([args[0].get_tail()])

def index(args):
    """
    Gets the value of a list at an index
    """
    # input validation
    if len(args) != 2 or args[1] < 0:
        raise CarlaeEvaluationError("Incorrect indexing method")
    if not islist([args[0]]):
        if isinstance(args[0], Pair) and args[1] == 0:
            return args[0].get_head()
        raise CarlaeEvaluationError("Incorrect indexing method")

    return index_pair(args[0], args[1]).get_head()

def index_pair(lst, index):
    """
    Helper function for getting the pair at an index
    """
    # input validation
    if lst == "nil":
        raise CarlaeEvaluationError("Index out of bound")
    # base case
    if index == 0:
        return lst
    # recursive call on tail
    return index_pair(lst.get_tail(), index - 1)

def concat(args):
    """
    Concatenates lists
    """
    # basic checks
    if not args:
        return "nil"
    if not islist([args[0]]):
        raise CarlaeEvaluationError("Inputs must be lists")
    if len(args) == 1:
        return args[0].copy() if args[0] != "nil" else args[0] # base case
    if args[0] == "nil":
        return concat(args[1:]) # skip over "nil" lists

    lst = args[0].copy()
    index_pair(lst, length([lst]) - 1).set_tail(concat(args[1:])) # recurse
    return lst

def map(args):
    """
    Applies a given function to each value in a list
    """
    # input validation
    if len(args) != 2 or not callable(args[0]) or not islist([args[1]]):
        raise CarlaeEvaluationError("Please enter a function and a list")
    if args[1] == "nil":
        return "nil"

    out = args[1].copy()
    for i in range(length([out])): # traverse list
        index_pair(out, i).set_head(args[0]([index([out, i])])) # change each element
    return out

def filter(args):
    """
    Removes values in a list that do not satisfy a given condition
    """
    # input validation
    if len(args) != 2 or not callable(args[0]) or not islist([args[1]]):
        raise CarlaeEvaluationError("Please enter a function and a list")
    
    lst = args[1]
    out = "nil"
    for i in range(length([lst])): # traverse list
        val = index([lst, i])
        if args[0]([val]):
            out = concat([out, Pair(val, "nil")]) # concatenate correct element to list
    return out

def reduce(args):
    """
    Applies a function to all element in a list from a start value
    """
    # input validation
    if len(args) != 3 or not callable(args[0]) or not islist([args[1]]):
        raise CarlaeEvaluationError("Please enter correct inputs")
    
    out = args[2]
    for i in range(length([args[1]])): # traverse list
        out = args[0]([out, index([args[1], i])]) # apply to each value
    return out

def begin(args):
    """
    Evaluates args and returns the result of the last one
    """
    # input validation
    if not args:
        raise CarlaeEvaluationError("Must enter some commands")
    
    return args[-1]

    # for arg in args:
    #     out = evaluate(arg) # evaluate each argument
    # return out # return last arg evaluated


mul = lambda args: args[0] if len(args) == 1 else (args[0] * mul(args[1:]))

carlae_builtins = {
    "+": sum,
    "-": lambda args: -args[0] if len(args) == 1 else (args[0] - sum(args[1:])),
    "*": mul,
    "/": lambda args: args[0] if len(args) == 1 else (args[0] / mul(args[1:])),

    "@t": True,
    "@f": False,

    "=?": comparisons("equal"),
    ">": comparisons("dec"),
    ">=": comparisons("non_inc"),
    "<": comparisons("inc"),
    "<=": comparisons("non_dec"),
    "not": comparisons("not_func"),

    "nil": "nil",
    "pair": pair,
    "head": head,
    "tail": tail,
    "list": list_func,
    "list?": islist,
    "length": length,
    "nth": index,
    "concat": concat,
    "map": map,
    "filter": filter,
    "reduce": reduce,
    "begin": begin
}


##############
# Evaluation #
##############

default = Environment(None, carlae_builtins)

def var(tree, env):
    """
    Evaluates the value to be assigned to our variable and adds it
    to the proper environment
    
    Will never be called without an environment passed in, since
    it is a helper for evaluate
    """
    if len(tree) > 3:
        raise CarlaeEvaluationError("Too many arguments for variable assignment")
    var = tree[1] # variable name
    if isinstance(var, list): # easier function definitions
        new_var = var[0]
        new_val = Function(env, var[1:], tree[2])
        env.set_val(new_var, new_val)
        return new_val
    
    val = evaluate(tree[2], env) # evaluate variable value
    env.set_val(var, val) # bind in environment
    return val


def conditional(tree, env):
    condition = tree[1]
    if evaluate(condition, env):
        return evaluate(tree[2], env)
    return evaluate(tree[3], env)

def let(tree, env):
    definitions = tree[0]
    vars = dict()
    for var, val in definitions:
        vars[var] = evaluate(val, env)
    new_env = Environment(env, vars)
    return evaluate(tree[1], new_env)


def evaluate(tree, env=None):
    """
    Evaluate the given syntax tree according to the rules of the Carlae
    language.

    Arguments:
        tree (type varies): a fully parsed expression, as the output from the
                            parse function 
    """
    # create an environment if it doesn't exist already
    if not env:
        env = Environment(default)

    ######## NOT S-EXPRESSION ########
    if not isinstance(tree, list): # if not a list
        if isinstance(tree, str):
            return env.get_val(tree)
        return tree
    
    ######## CHECK FOR SPECIAL FORM ########
    if not tree:
        raise CarlaeEvaluationError("Empty expression")

    op = tree[0] # operation
    
    # variable definition
    if op == ":=":
        return var(tree, env)

    # creating a function
    if op == "function":
        return Function(env, tree[1], tree[2])

    # if conditional
    if op == "if":
        return conditional(tree, env)

    # and conditional
    if op == "and":
        for condition in tree[1:]:
            if not evaluate(condition, env):
                return False
        return True

    # or conditional
    if op == "or":
        for condition in tree[1:]:
            if evaluate(condition, env):
                return True
        return False
    
    # del var special form
    if op == "del":
        return env.del_val(tree[1])

    # let special form
    if op == "let":
        return let(tree[1:], env)

    # set bang special form
    if op == "set!":
        return env.set_bang(tree[1], evaluate(tree[2], env))

    ######## NOT SPECIAL FORM ########
    func = evaluate(op, env)
    if not callable(func):
        raise CarlaeEvaluationError("First argument is not a function")
    return func([evaluate(elt, env) for elt in tree[1:]])


def result_and_env(tree, env=None):
    """
    Evaluate function that also returns the environment it was evaluated in
    """
    # create an environment if it doesn't exist already
    if not env:
        env = Environment(default)
    return evaluate(tree, env), env # also return environment


def evaluate_file(filename, env=None):
    if not env:
        env = Environment(default)
    f = open(filename)
    return evaluate(parse(tokenize(f.read())), env)


def REPL(env=None):
    # create an environment if it doesn't exist already
    if not env:
        env = Environment(default)
    while True:
        exp = input("in> ")
        if exp == "EXIT":
            break
        try:
            print("  out> " + str(evaluate(parse(tokenize(exp)), env)))
        except CarlaeError as e:
            print(e)

######## QUESTION ########
# Note that in the case of an S-expression that isn't a special form
# evaluating the first element in the S-expression gives us the function that is to be called, 
# regardless of how it is specified (so your evaluator code should not have additional logic based on the syntactic form of that first element).



if __name__ == "__main__":
    # code in this block will only be executed if lab.py is the main file being
    # run (not when this module is imported)

    # uncommenting the following line will run doctests from above
    # doctest.testmod()

    env = Environment(default)
    args = sys.argv
    while len(args) > 1:
        print(evaluate_file(args.pop(-1), env))

    REPL(env)