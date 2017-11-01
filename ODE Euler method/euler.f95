subroutine euler_solver(x, y, df, x0, y0, xf, n)
! Solve ODE using Euler's method.
! Assumes a function named 'derivative_func' that provides first derivative values
! ---------
! Arguments
! ---------
! x  : array of x values
! y  : array of y values
! df : array of first derivative values at x, y! x0 : initial x value
! y0 : initial y value
! xf : final x value
! n  : number of steps
	external :: derivative_func
	real, dimension(0:50) :: x, y, df
	h = (xf - x0)/real(n)
	eps = 1.0E-09
	x(0) = x0
	y(0) = y0
	df(0) = derivative_func(x0, y0)
	do  i = 1, n
		x(i) = x0 + i*h
		y(i) = y(i-1) + df(i-1) * h
		do
			df(i) = derivative_func(x(i), y(i))
			y_new = y(i-1) + (df(i) + df(i-1)) * h * 0.5
			if( abs(y(i) - y_new) <= eps ) then
				exit
			end if
			y(i) = y_new
		end do
	end do
end subroutine euler_solver