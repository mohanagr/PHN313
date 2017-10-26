subroutine gauss_eli(A, X, B, n)
	dimension A(50, 50), X(50), B(50)

	!Forward elimination

	do i = 2, n
		!write(*,*) A(i, i-1)
		do j = i, n
			val = A(j, i-1)/A(i-1, i-1)
			A(j, :) = A(j, :) - A(i-1, :)*(A(j, i-1)/A(i-1, i-1))
			B(j) = B(j) - B(i-1)*val
			write(*,*) "B(j) is", B(j)
		end do
	end do
	write(*,*) B

	!Back substitution
	do i = n, 1, -1
		s = 0.0
		do j = 0, n-i-1
			write(*,*) "i is:" , i
			s = s + A(i, n-j)*X(n-j)
		end do
		X(i) = (B(i) - s)/A(i, i) ! Upper triangular matrix
	end do

end subroutine