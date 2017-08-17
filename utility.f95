function differentiate(f, x)
	h = 1.0E-06
	f_prime = (f(x + h) - f(x - h))/ (2*h)
	return f_prime
end function