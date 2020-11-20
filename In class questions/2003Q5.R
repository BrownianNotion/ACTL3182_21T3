risk_neutral <- function(r, u, d) {
  (exp(r) - d)/(u - d)
}
r = 0.04
u = 1.2
d = 0.85

#part 1
14.4 - 1/3 *(10+12+14.4)
f7 = 10*u^2 - 10/3 * (1 + u + u^2)

f6 = 10*u - 10/3 * (1 + u + u*d)

f5 = 10*u*d  - 10/3 * (1 + d + u*d)

f4 = 10 - 10/3 * (1 + d + d^2)

phi1u = (f7 - f6)/(10*u^2 - 10*u*d)
phi1u
psi1u = exp(-2*r)*(f7 - phi1u * 10*u^2)
psi1u

phi1d = (f5 - f4)/(10*u*d - 10*d^2)
phi1d 
psi1d = exp(-2*r)*(f5 - phi1d*10*u*d)
psi1d



f3 = phi1u * 10*u + psi1u*exp(r)
f3
f2 = phi1d * 10*d + psi1d*exp(r)
f2

phi0 = (f3 - f2)/(10*u - 10*d)
phi0
psi0 = exp(-r)*(f3 - phi0*10*u)
psi0

f1 = phi0*10 + psi0
f1

#Using risk-neutral probabilities:
q <- risk_neutral(r, u, d)
price_q = exp(-2*r)*(q^2*f7 + q*(1-q)*(f6 + f5) + (1-q)^2*f4)
price_q
