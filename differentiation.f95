subroutine diff(df, f, x, n)
	real:: df(50), f(50), x(50)
	h = x(2) - x(1)
	df(1) = (-3.0*f(1) + 4.0*f(2) - f(3))/(2.0*h)
	df(n) = (f(n-2) - 4.0*f(n-1) + 3.0*f(n))/(2.0*h)

	do i=2,n-1
		df(i) = (f(i+1) - f(i-1))/(2.0*h)
	end do
end subroutine

program differentiate
	
	integer :: stat
	dimension x(50), f(50), df(50), ddf(50)
	n = 1
	open(unit=8, file='differentiation.in', status='unknown')
	do
		!write(*,*) 'reading...'
		read(8, '(2f11.5)', IOSTAT=stat) x(n), f(n)
		!write(*,*) x(index), y(index)
		if (stat < 0) then
			exit
		else if (stat > 0) then
			write(*,*) "Something went wrong while reading the file!"
			exit
		else
			n = n + 1
		end if
	end do
	close(8)
	n = n - 1
	call diff(df, f, x, n)

	write(*,*) "The first derivative values at each point are :"
	write(*,*) (df(i),i=1,n)

	call diff(ddf, df, x, n)

	write(*,*) "The second derivative values at each point are :"
	write(*,*) (ddf(i),i=1,n)

end program differentiate


	
