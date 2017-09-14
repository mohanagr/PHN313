program lagrangei

real, external :: Lagrange
integer :: stat
dimension x(50), y(50)
n = 1
open(unit=8, file='lagrange_interpol.in', status='unknown')
do
	!write(*,*) 'reading...'
	read(8, '(2f11.5)', IOSTAT=stat) x(n), y(n)
	!write(*,*) x(index), y(index)
	if (stat < 0) then
		exit
	else if (stat > 0) then
		write(*,*) "Something went wrong while reading the file!"
		exit
	else
		n = n + 1
	end if
end do
close(8)
n = n - 1

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter x value:"
read(*,*) xx

val = Lagrange(x, y, xx, n)

write(*,'(A, f7.4, A, f9.4)') "The value at ", xx, " is", val

end program lagrangei