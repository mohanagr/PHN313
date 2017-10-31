subroutine gauss_seidel(A, X, B, n, niter, outfile)
	dimension A(50, 50), X(50), B(50), err(50)
	character(*) :: outfile
	open(unit = 10, file = outfile, status='unknown')
	do i = 1, niter

		do j = 1, n

			s = 0.0

			do k = 1, n
				if (j == k) then
					cycle
				endif
				s = s + A(j, k) * X(k)
			end do
			!write(*,*) "sum is", s, "B is", B(j), "Denom is", A(j, j)
			Xini = X(j) 
			X(j) = (B(j) - s)/A(j,j)
			!write(*,*) "X", j, "is", X(j), "Xini is ", Xini
			err(j) = abs( (X(j) - Xini)/X(j) )
		end do
		write(10, '(6F10.4)') (X(m), err(m), m = 1, n)
	end do

	close(10)

end subroutine


