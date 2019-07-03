def fast_exp(a, n):
  if n == 1:
    return a
  if n % 2 == 0:
    return fast_exp(a, n // 2) ** 2
  else:
    return a * (fast_exp(a, (n-1) // 2) ** 2)

# solution
def fast_exp_tr(a, n, acc):
  if n == 0:
    return acc
  if n % 2 == 0:
    return fast_exp_tr(a**2, n // 2, acc)
  else:
    return fast_exp_tr(a, n-1, a * acc)

print(fast_exp(2, 3))
print(fast_exp_tr(2, 3, 1))
print(fast_exp_tr(2, 5, 1))