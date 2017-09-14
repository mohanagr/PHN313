
program differentiate
	
	real, external :: Lagrange
	integer :: stat, len
	dimension x(50), xx(50), f(50), y(50), df(50), ddf(50)
	n = 1
	write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter the difference h value:"
	read (*, *) h

	open(unit=9, file='diff.out', status='unknown')
	open(unit=8, file='differentiation2.in', status='unknown')
	do
		!write(*,*) 'reading...'
		read(8, '(2f11.5)', IOSTAT=stat) x(n), y(n)
		if (stat < 0) then
			exit
		else if (stat > 0) then
			write(*,*) "Something went wrong while reading the file!"
			exit
		else
			write(9, '(2f11.3)') x(n), y(n)
			n = n + 1	
		end if
	end do
	close(8)
	close(9)
	n = n - 1

	len = ((x(n) - x(1))/h) + 1


	do i = 1, len
		xx(i) = x(1) + real((i-1))*h
		f(i) = Lagrange(x, y, xx(i), n)
	end do

	call diff(df, f, xx, len)

	call diff(ddf, df, xx, len)

	open(unit=9, file='differentiation2.out', status='unknown')
	write(9, "(4F11.5)") (xx(j), f(j), df(j), ddf(j), j = 1,len) 
	close(9)

end program differentiate

subroutine diff(df, f, x, n)
	real:: df(50), f(50), x(50)
	h = x(2) - x(1)
	df(1) = (-3.0*f(1) + 4.0*f(2) - f(3))/(2.0*h)
	df(n) = (f(n-2) - 4.0*f(n-1) + 3.0*f(n))/(2.0*h)

	do i=2,n-1
		df(i) = (f(i+1) - f(i-1))/(2.0*h)
	end do
end subroutine

	
