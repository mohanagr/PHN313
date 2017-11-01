function f(x)
	f = x * exp(x) + x**2
	return
end function

program simpsons

real :: x(20000), y(20000)

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter a, b and no. of iterations (max 10) :"

read (*, *) a, b, iter

n = 10000
h = (b - a)/float(n)

area_real = exp(2.0) + 11.0/3.0

do i = 1, n
	x(i) = a + i * h
	y(i) = f(x(i))
end do

do j = 1, iter
	write(*,'(A, I2, A, I6)') "Iteration", j, " for n = ", n
	area = 0.0
	do i = 1, n-1, 2
		area = area + 4 * f(x(i)) + 2 * f(x(i+1))
	end do
	area = ((area + f(a) - f(b))*h)/(3.0)

	write(*,'(A, f11.6)') "Estimated integral is ", area
	write(*,'(A, f11.6)') "Absoulte relative error is ", abs((area_real - area)/area_real)
	write(*,'(A, f11.6)') "Real area is ", area_real
	n = n + 1000
end do

end program simpsons





