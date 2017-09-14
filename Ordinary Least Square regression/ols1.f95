program ols
	external linenumber
	real:: x(50), y(50), beta(0:50), rmse
	integer :: stat
	write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter x value where to predict:"
	read (*, *) xx

	n = linenumber('ols1.in')
	write(*,*) n, "lines"
	open(unit=8, file='ols1.in', status='unknown')
	read(8, '(2f11.5)', IOSTAT=stat) x(index), y(index)
	close(8)

	do i = 1, n
		y(i) = log(y(i))
		!write(*,*) y(i)
	end do

	call fit(x, y, beta, n, rmse)

	y_predict = predict(beta, xx)

	y_predict = exp(y_predict) !TRANSFORMING AGAIN

	write(*,'(A, f11.5, A, f11.5, A, f11.5)') "The value at", xx, " is", y_predict, " with RMSE", rmse

end program ols


function predict(beta, x)
	real :: beta(0:50), x
	predict = beta(0) + beta(1) * x
	return
end function

subroutine fit(x, y, beta, n, rmse)
	real :: x(50), y(50), beta(0:50), xysum = 0.0, xsum = 0.0, ysum =0.0, xsquaresum = 0.0
	!n = size(y)
	do i = 1, n
		ysum = ysum + y(i)
		xsum = xsum + x(i)
		xsquaresum = x(i)**2
		xysum = x(i)*y(i)
	end do
	xmean = xsum/float(n)
	ymean = ysum/float(n)
	beta(1) = (n*xysum - xsum*ysum)/(n*xsquaresum - xsum**2)
	beta(0) = ymean - beta(1)*xmean
	!write(*,*) ymean, xmean
	rmse = 0.0
	do i = 1, n
		y_p = predict(beta, x(i))
		rmse = rmse + (y(i) - y_p)**2
	end do
	rmse = (rmse/n)**0.5
end subroutine

