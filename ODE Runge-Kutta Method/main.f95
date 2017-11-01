program rungekutta
	
	external :: RK1
	real, dimension(0:50) :: x, y

	write(*,*) "Please enter the initial x and y values"
	read(*,*) x0, y0
	write(*,*) "Please enter the number of interations"
	read(*,*) n
	write(*,*) "Please enter the target x (final)"
	read(*,*) xf

	call RK1(x, y, x0, y0, xf, n)

	write(*,*) "The value at x =", xf, "is ", y(n)

end program rungekutta

	




