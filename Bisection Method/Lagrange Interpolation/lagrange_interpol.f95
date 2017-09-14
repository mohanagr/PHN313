function Lagrange(x, y, xx, n)
	real :: x(50), y(50), fn, z
	!character(*) :: datafile
	fn = 0.0
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