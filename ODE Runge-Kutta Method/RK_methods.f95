subroutine RK1(x, y, x0, y0, xf, n)
! Solve first order ODE of type
! dy/dx = f(x, y)
! Assumes external funtion "f"

	real, dimension(0:50) :: x, y

	h = (xf - x0)/real(n)
	x(0) = x0
	y(0) = y0

	do i = 1, n
		k1 = f(x(i - 1), y(i - 1)) * h
		k2 = f((x(i - 1) + h / 2.0), (y(i - 1) + k1 / 2.0)) * h
		k3 = f((x(i - 1) + h / 2.0), (y(i - 1) + k2 / 2.0)) * h
		k3 = f((x(i - 1) + h), (y(i - 1) + k3)) * h

		del_y = (k1 + 2.0 * k2 + 2.0 * k3 + 2.0 * k4) / 6.0
		y(i) = y(i - 1) + del_y
		x(i) = x(0) + i * h
	end do

end subroutine

subroutine RK2(x, y, df, x0, y0, df0, xf, n)
! Solve second order ODE of type
! d2y/d2x = f(x, y, y')
! Assumes external funtion "f"

	real, dimension(0:50) :: x, y, df

	h = (xf - x0)/real(n)
	x(0) = x0
	y(0) = y0
	df(0) = df0
	do i = 1, n
		k1 = f(x(i - 1), y(i - 1), df(i - 1)) * h
		k2 = f((x(i - 1) + h / 2.0), (y(i - 1) + df(i - 1) * h / 2.0 + h * k1 / 8.0), (df0(i - 1) + k2 / 2.0)) * h
		k3 = f((x(i - 1) + h / 2.0), (y(i - 1) + k2 / 2.0)) * h
		k3 = f((x(i - 1) + h), (y(i - 1) + k3)) * h

		del_y = (k1 + 2.0 * k2 + 2.0 * k3 + 2.0 * k4) / 6.0
		y(i) = y(i - 1) + del_y
		x(i) = x(0) + i * h
	end do

end subroutine
