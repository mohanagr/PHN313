function f(x)
	f = x**2 + 2*x + 1
	return
end function

program gauss

real :: x(20), weights(20)
integer :: stat, index = 1

open(unit=8, file='gaussian_quad.in', status='unknown')
open(unit=9, file='gaussian_quad.out', status='unknown')
do
	!write(*,*) 'reading...'
	read(8, '(2f20.16)', IOSTAT=stat) weights(index), x(index) 
	write(9,'(2f20.16)') weights(index), x(index)
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
close(9)

n = index - 1

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter a, b :"
read(*, *) a, b
slopem = (b - a)/(2.0)
const = (b + a)/2.0
area_real = 39.0
area = 0.0
do i = 1, n
	xx = slopem*x(i) + const
	area = area + (weights(i)*f(xx))
end do
area = area * ((b - a)/2.0)

write(*,'(A, f14.10)') "Estimated integral is ", area
write(*,'(A, f14.10)') "Absoulte relative error is ", abs((area_real - area)/area_real)


end program gauss





