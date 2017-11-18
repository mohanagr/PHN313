program gauss_main
	
	external :: inv_eli
	dimension A(50, 50), X(50, 50)
	write(*,*) "Enter number of rows:"
	read(*,*) n
	open(unit=8, file='inv.in', status='unknown')
	open(unit=10, file='inv.try', status='unknown')
	read(8,'(3F10.4)') ((A(i, j), j = 1, n), i = 1, n)
	write(10,'(3F10.4)') ((A(i, j), j = 1, n), i = 1, n)
	close(10)
	close(8)
	call inv_eli(A, X, n)
	open(unit=9, file='inv.out', status='unknown')
	write(9,'(3F10.4)') ((X(i, j), j = 1, n), i = 1, n)
	close(9)

end program gauss_main

