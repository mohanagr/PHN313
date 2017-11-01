subroutine gauss_eli(A, X, B, n)
	dimension A(50, 50), X(50), B(50)

	!Forward elimination
	!call replace_rows(A, B, 1, n)
	do i = 2, n
		!write(*,*) A(i, i-1)
		!call replace_rows(A, B, i, n)
		do j = i, n
			val = A(j, i-1)/A(i-1, i-1)
			A(j, :) = A(j, :) - A(i-1, :)*(A(j, i-1)/A(i-1, i-1))

			! LET L ==> LOWER TRIANGULAR MATRIX FOR LUD METHOD
			! THEN add following to get L filled apart from diagonal of 1's and rest 0's
			! L(j, i-1) = val
			B(j) = B(j) - B(i-1)*val
			!write(*,*) "B(j) is", B(j)
		end do
	end do
	!write(*,*) B

	!Back substitution
	do i = n, 1, -1
		s = 0.0
		do j = i+1, n
			!write(*,*) "i is:" , i
			s = s + A(i, j)*X(j)
		end do
		X(i) = (B(i) - s)/A(i, i) ! Upper triangular matrix
	end do

end subroutine

! Partial pivoting to avoid division by zero
subroutine replace_rows(A, B, k, n)

	dimension A(50, 50), temp(50), B(50)
	real :: max
	max = -1.0E12
	do l = k, n
		val = abs(A(l,k))
		!write(*,*) "val is ", val 
		if( val >= max) then
			max = val
			max_index = l
		end if
	end do
	!write(*,*) max, max_index
	temp(:) = A(k, :)
	A(k, :) = A(max_index, :)
	A(max_index, :) = temp(:)

	temp1 = B(k)
	B(k) = B(max_index)
	B(max_index) = temp1

end subroutine


