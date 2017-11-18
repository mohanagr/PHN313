subroutine inv_eli(A, X, n)
	dimension A(50, 50), X(50, 50), B(50, 50)

	!Forward elimination
	!call replace_rows(A, B, 1, n)
	do m = 1, n
		do l = 1, n
			if (l == m) then
				B(m, l) = 1.0
			else
				B(m, l) = 0.0
			end if
		end do
	end do

	do i = 2, n
		!call replace_rows(A, B, i, n)
		do j = i, n
			val = A(j, i-1)/A(i-1, i-1)
			A(j, :) = A(j, :) - A(i-1, :)*val

			! Multidimensional B, to solve 3 LEQ simultaneously.
			B(j, :) = B(j, :) - B(i-1, :)*val
			!write(*,*) "B(j) is", B(j)
		end do
	end do

	!Back substitution
	do m = 1, n
		do i = n, 1, -1
			s = 0.0
			do j = i+1, n
				s = s + A(i, j)*X(j, m)
			end do
			X(i, m) = (B(i, m) - s)/A(i, i)
			!write(*,*) "X(i,m) is :", X(i,m)
		end do
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


