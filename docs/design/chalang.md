# Chalang

[Chalang](https://github.com/aeternity/chalang) is a stack-based language implementing smart contracts.

As an example, here is the code used for lightning payments, annotated:

## lightning code

It is expected to return a stack with at least 4 values: [ShareRoot, Amount, Nonce, Delay | _].

```
stack_size int 0 ==
if
int 50 int 1 int 0 nil crash
else then
drop drop hash binary <hash_size> <hash> print == swap drop swap drop 
if int 0 int 2 int 10000
else int 49 int 1 int 0
then nil crash
```

*   `stack_size`
    measures the current size of the hash and pushes it on (the top of) the stack as a number
*   `int 0`
    pushes the integer 0 on the stack
*   `==`
    compares the last two elements of the stack. pushes 1 (true) if they are equal, 0 otherwise. Compared elements are kept on the stack.
*   `if`
    removes top of the stack and, if nonzero, executes the code following up to the next `else` statement, otherwise the code after `else` up to a `then` statement
*   `int 50 int 1 int 0`
    stack content will be [0, 1, 50,...]
*   `nil`
    pushes an empty list [], returning the result of [shareRoot, amount, nonce, delay] = [[], 0, 1, 50,...] in this case.
*   `crash`
    finishes code execution and keeps the current stack which contains [[], 0, 1, 50, ...] at this point.
*   `else`
    followed by the code to be executed when the stack is non-empty. Nothing to execute in this case.
*   `then`
*   `drop drop`
    Removes the two elements added to the stack: stack size and 0
*   `hash`
    calculates the hash of the value that was at top at the beginning of execution. This is supposed to be the secret.
*   `binary <hash_size> <hash>`
    pushes the hash we expect
*   `print`
    does nothing except printing the hash expected
*   `==`
    pushes 1 if hashes match, 0 otherwise
*   `swap drop swap drop`
    removes the two values compared, keeps the result of comparison at top
*   `if int 0 int 2 int <granularity>`
    if hashes match, results in [`<granularity>`, 2, 0, ...] in stack. `<granularity>` is defined in `constants:channel_granularity()`, it is the amount of the bet.
*   `else int 49 int 1 int 0`
    if hashes do not match results in [0, 1, 49] in stack
*   `then nil crash`
    pushes an empty list to the top of the stack in both cases and finishes execution. The result will be [shareRoot, amount, nonce, delay] = [[], 10000, 2, 0] for matching hash and [[], 0, 1, 49] otherwise.

## State and data

*   state contains
    *   height: the number of blocks currently in chain
    *   slash:  true if running in a solo_close or slash transaction, false otherwise

*   data
    stores limits and actual values for time and space gas, functions and variables, the list of secrets, the bet code, the chalang state and the hash size to be used.

## Invoking

### Initialization

1.  `new_state/2, 3`
    Creates initial state

2.  `data_maker/8`
    Initializes Chalang data used in run steps

### Run

1.  `run5/2`
    *   checks well-formedness
    *   calls `run2/2`

2.  `run2/2`
    *   parses function calls and defitions, binaries and conditionals
    *   calls `run4/2`

3.  `run4/2`
    Parses basic constructs

### Retrieving result

1.  `stack/1`
    Retrieves stack from final chalang data.
