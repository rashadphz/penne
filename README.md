# Penne
Partial Python compiler written in OCaml. Penne compiles Python programs to LLVM Intermediate Representation which can then be interpreted or converted to machine language. With LLVM Optimizations, Penne can make Python programs run with the speed of C or C++.

## Limitations
The Penne compiler is not meant to match the flexibility of CPython. Penne is limited to a small subset of Python functionality. It was an attempt to learn LLVM and OCaml simultaneously and serve as an example of how fast compiled Python can be.
  ### Supports: 
  - Multiple argument functions
  - Basic array initialization and element access (No modification)
  - Conditional If Else Blocks 
  - While Loops
  - Python style indentation
  - Arithmetic
  ### Important but not yet implemented:
  - Type system (currently only signed 32 bit integers)
  - Classes
  - Dynamic lists
  - List comprehensions
  - For loops
  - Sets/Hashtables

## Comparisons
Time in seconds of Penne includes compile time and run time.

### Fibonacci of 44 (Recursive)
```
def fib(n):
  if n <= 2:
    return 1
  else:
    return fib(n - 1) + fib(n - 2)

print(fib(44))
```
**Python Interpreter: 77.15 seconds**

**Penne Compiler: 1.60 seconds**

### Greatest Common Denominator (Iterative)
```
def gcd(a, b):
  while a != b:
      if a > b:
          a = a - b
      else:
          b = b - a
  return a

print(gcd(2147483648, 2))
```
**Python Interpreter: 44.92 seconds**

**Penne Compiler: 0.845 seconds**



