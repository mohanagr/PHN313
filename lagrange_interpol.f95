program lagrangei

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter x value:"
read (*, *) xx
val = Lagrange('lagrange_interpol.in', xx)
write(*,'(A, f5.2, A, f8.4)') "The value at", xx, " is", val

end program lagrangei

function Lagrange(datafile, xx)
	real :: x(50), y(50), fn = 0.0
	character(*) :: datafile
	integer :: stat
	index = 1
	open(unit=8, file='lagrange_interpol.in', status='unknown')
	do
		!write(*,*) 'reading...'
		read(8, '(2f11.5)', IOSTAT=stat) x(index), y(index)
		write(9, '(2f11.5)') x(index), y(index)
		!write(*,*) x(index), y(index)
		if (stat < 0) then
			exit
		else if (stat > 0) then
			write(*,*) "Something went wrong while reading the file!"
			exit
		else
			index = index + 1
		end if
	end do
	close(8)
	n = index - 1
	do i = 1, n
		prod = 1.0
		do j = 1, n	
			if(j==i) then
				cycle
			end if
			prod = prod * ((xx - x(j)) / (x(i) - x(j)))
		end do
		fn = fn + prod * y(i)
	end do
	Lagrange = fn
	return
end function Lagrange