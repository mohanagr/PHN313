
function differentiate(f, x)
	h = 1.0E-06
	f_prime = (f(x + h) - f(x - h))/ (2*h)
	return
end function

function linenumber(datafile)
	character(*) :: datafile
	open(unit=5, file=datafile, status='unknown')
	index = 1
	do
		read(*,*) x, y
		if (stat < 0) then
			exit
		else if (stat > 0) then
			write(*,*) "Something went wrong while reading the file!"
			exit
		else
			index = index + 1
		end if
	end do
	close(5)
	linenumber = index - 1
	return
end function linenumber
