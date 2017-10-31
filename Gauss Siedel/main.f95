program seidel_main
	
	external :: gauss_seidel
	dimension A(50, 50), X(50), Xini(50), B(50)
	write(*,*) "Enter number of rows:"
	read(*,*) n
	write(*,*) "Enter number of iterations:"
	read(*,*) niter
	open(unit=8, file='seidel.in', status='unknown')
	open(unit=10, file='seidel.try', status='unknown')
	read(8,'(5F10.4)') ((A(i, j), j = 1, n), B(i), X(i), i = 1, n)
	write(10,'(5F10.4)') ((A(i, j), j = 1, n), B(i), X(i), i = 1, n)
	close(10)
	close(8)

	call gauss_seidel(A, X, B, n, niter, 'seidel.out')

	! open(unit=9, file='seidel.out', status='unknown')
	! write(9,'(5F10.4)') ((A(i, j), j = 1, n), B(i), X(i), i = 1, n)
	! close(9)

end program seidel_main

