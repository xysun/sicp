'''
p' = p**2 + q**2
q' = q**2 + 2*p*q
'''

def fib_iter(a,b,p,q,count):
  if count == 0:
    return b
  else:
    if count % 2 == 0:
      return fib_iter(a,b,p**2+q**2, q**2 + 2*p*q, count // 2)
    else:
      return fib_iter(b*q+a*q+a*p, b*p+a*q, p, q, count-1)

def fib(n):
  return fib_iter(1,0,0,1,n)

for i in range(10):
  print(fib(i))