program ols
	real:: x(50), y(50), beta(0:50), rmse, cc

	write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter number of data points:"
	read (*, *) n
	write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter x value where to predict:"
	read (*, *) xx
	open(unit=8, file='ols2.in', status='unknown')
	read(8, '(2f11.5)') (x(index), y(index), index = 1, n)
	close(8)

	! Do the required preprocessing of data here if needed
	! E.g. converting y to log(y) etc.

	! do i = 1, n
	! 	y(i) = log(y(i))
	! 	!write(*,*) y(i)
	! end do

	! Linear fit only
	call fit(x, y, beta, n, rmse, cc)

	y_predict = predict(beta, xx)

	! y_predict = exp(y_predict) !TRANSFORMING AGAIN

	write(*,'(A, f10.5, A, f10.5, A, f10.5, A, f10.5)') "The value at", xx, " is", y_predict, " with RMSE", rmse, " and cc", cc

	write(*, '(A, 2F10.4)') "The coefficients are :", beta(0), beta(1)

end program ols


function predict(beta, x)
	real :: beta(0:50), x
	predict = beta(0) + beta(1) * x
	return
end function

subroutine fit(x, y, beta, n, rmse, cc)
	real :: x(50), y(50), beta(0:50), xysum = 0.0, xsum = 0.0, ysum =0.0, xsquaresum = 0.0
	!n = size(y)
	do i = 1, n
		ysum = ysum + y(i)
		xsum = xsum + x(i)
		xsquaresum = xsquaresum + x(i)**2
		xysum = xysum + x(i)*y(i)
	end do
	xmean = xsum/float(n)
	ymean = ysum/float(n)
	beta(1) = (n*xysum - xsum*ysum)/(n*xsquaresum - xsum**2)
	beta(0) = ymean - beta(1)*xmean
	!write(*,*) ymean, xmean
	rmse = 0.0
	Sy = 0.0
	S = 0.0
	do i = 1, n
		y_p = predict(beta, x(i))
		Sy = Sy + (y(i) - ymean)**2
		S = rmse + (y(i) - y_p)**2
	end do
	rmse = (S/float(n))**0.5
	cc = ((Sy - S)/Sy)**0.5
end subroutine

