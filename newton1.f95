program newton1

f(x) = 4*exp(-x)*sin(x) - 1

f_prime(x) = 4.0 *exp(-x)*(cos(x) - sin(x))

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter an initial guess:"
read (*, *) x
write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter the number of iterations:"
read (*, *) n
eps = 1.0E-07
del = 1
flag = 0

! Use a do loop with fixed number of iterations because
! Newton-Rhapson method is not guaranted to converge in
! certain cases

do i=1,n
	if (eps > abs(del)) then
		flag = 1
		exit
	end if
	y = f(x)
	y_prime = f_prime(x)
	x_new = x - y/y_prime
	!print *, "x new is", x_new
	del = (x_new - x)/x
	x = x_new
end do

if (flag == 1) then
	write(*,*) "Newton-Rhapson method converged at root", x
else
	write(*,'(A,1I2,A)') "Newton-Rhapson could not converge in", n, " iterations. Please increase n or change the initial guess"
end if

end program newton1