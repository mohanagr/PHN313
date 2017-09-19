function derivative_func(x, y)
	derivative_func = x + y
	return
end function

program euler_main

external :: euler_solver
real, dimension(0:50) :: x, y, df
write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter initial x, y, final x and steps:"
read(*, *) x0, y0, xf, n

call euler_solver(x, y, df, x0, y0, xf, n)

open(unit=8, file="euler.out", status="unknown")

write(8, '(3F9.4)') (x(i), y(i), df(i), i = 0, n)

close(8)

end program euler_main


