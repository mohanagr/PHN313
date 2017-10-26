program gauss_main
	
	external :: gauss_eli
	dimension A(50, 50), X(50), B(50)
	write(*,*) "Enter number of rows:"
	read(*,*) n
	open(unit=8, file='gauss.in', status='unknown')
	open(unit=10, file='gauss.try', status='unknown')
	read(8,'(4F10.4)') ((A(i, j), j = 1, n), B(i), i = 1, n)
	write(10,'(4F10.4)') ((A(i, j), j = 1, n), B(i), i = 1, n)
	close(10)
	close(8)
	call gauss_eli(A, X, B, n)
	open(unit=9, file='gauss.out', status='unknown')
	write(9,'(5F10.4)') ((A(i, j), j = 1, n), B(i), X(i), i = 1, n)
	close(9)

end program gauss_main

