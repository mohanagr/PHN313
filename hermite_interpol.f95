program hermitei

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter x value:"
read (*, *) xx
y_prime = f_prime(xx)
val = hermite('hermite_interpol.in', xx, y_prime)
write(*,'(A, f5.2, A, f8.4)') "The value at", xx, " is", val

end program hermitei

function hermite(datafile, xx, y_prime)
	real :: x(50), y(50), fn = 0.0, Li, Li_prime
	character(*) :: datafile
	integer :: stat
	index = 1
	open(unit=8, file='hermite_interpol.in', status='unknown')
	do
		!write(*,*) 'reading...'
		read(8, '(2f11.5)', IOSTAT=stat) x(index), y(index)
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
		Li = 1.0
		Li_prime = 0.0
		do j = 1, n	
			if(j==i) then
				cycle
			end if
			Li = Li * ((xx - x(j)) / (x(i) - x(j)))
			Li_prime = Li_prime + 1/(x(i) - x(j))
		end do
		fn = fn + ((1 - 2*(xx - x(i)) * Li_prime) * ((Li)**2) * y(i) + (xx - x(i)) * ((Li)**2) * f_prime(x(i)))
	end do
	hermite = fn
	return
end function hermite

function f(x)
	y = x**3
end function

function f_prime(x)
	f_prime = 4*x**3
end function

