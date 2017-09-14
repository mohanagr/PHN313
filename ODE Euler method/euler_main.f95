! args: function, x array, y array, initial x, initial y, interval end, number of steps, 
subroutine euler_solver(x, y, x0, y0, xf, n)
	real, dimension(0:50) :: x, y, df
	h = (xf - x0)/real(n)
	eps = 1.0E-09
	x(0) = x0
	y(0) = y0
	df(0) = func(x0, y0)
	do  i = 1, n
		x(i) = x0 + (i-1)*h
		y(i) = y(i-1) + df(i-1) * h
		y_prev = 100000
		do
			if( abs(y(i) - y_prev) <= eps ) then
				exit
			end if
			y_prev = y(i)
			df(i) = func(x(i), y(i))
			y(i) = y(0) + (df(i) + df(i-1)) * h * 0.5
		end do
	end do
end subroutine euler_solver

function func(x, y)
	f = x + y
	return
end function

program euler_main

real, dimension(0:50) :: x, y, df
write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter initial x, y, final x and steps:"
read(*, *) x0, y0, xf, n

call euler_solver(x, y, x0, y0, xf, n)

open(unit=8, file="euler.out", status="unknown")

write(8, '(3F9.4)') (x(i), y(i), df(i), i = 0, n+1)

close(8)

end program euler_main


