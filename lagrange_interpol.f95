program lagrange

dimension x(50), y(50)

fn = 0.0

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter no. of data points and x value:"
read (*, *) n, xx
!write(*, *) n, xx
open(unit=8, file='lagrange_interpol.in', status='unknown')
open(unit=9, file='lagrange_interpol.out', status='unknown')
read(8, '(2f11.5)') (x(i), y(i),  i=1,n)
close(8)
! write(9, '(2f11.5)') (x(i), y(i),  i=1,n)
! close(9)
!write(*, *) "closed 6"
do i = 1, n
	!write(*,*) "p is", pval
	fn = fn + P(x, i, n, xx)*y(i)
end do

write(*,'(A, f5.2, A, f8.4)') "The value at", xx, " is", fn

end program lagrange

real function P(x, i, n, xx)
	dimension x(n)
	prod = 1.0
	do j = 1, n
		if(j==i) then
			cycle
		end if
		prod = prod * ((xx - x(j)) / (x(i) - x(j)))
	end do
	f = prod
	return
end function P