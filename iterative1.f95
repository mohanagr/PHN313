program iterative1

phi(x) = (sin(x) + 10)/10
!phi(x) = x**3 - 1

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter two space separated limiting points"
read (*, *) xl, xh
eps = 1.0E-07
del = 1

do while (abs(del) > eps)
	if (xl > xh) then
		write(*,*) "Limit exceeded and not converged"
		STOP
	end if
	xl_new = phi(xl)
	del = (xl_new - xl)/xl
	xl = xl_new
end do
write(*,*) "Iterative method converged at root", xl

end program iterative1