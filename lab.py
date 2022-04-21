#!/usr/bin/env python3
"""6.009 Lab 8: Carlae (LISP) Interpreter"""

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

mul = lambda args: args[0] if len(args) == 1 else (args[0] * mul(args[1:]))
carlae_builtins = {
    "+": sum,
    "-": lambda args: -args[0] if len(args) == 1 else (args[0] - sum(args[1:])),
    "*": mul,
    "/": lambda args: args[0] if len(args) == 1 else (args[0] / mul(args[1:]))
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

    REPL()
