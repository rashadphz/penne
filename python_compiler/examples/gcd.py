def gcd(a, b):
  while a != b:
      if a > b:
          a = a - b
      else:
          b = b - a
  return a

print(gcd(2147483648, 2))