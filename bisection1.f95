program bisection

f(x) = x**3 - x - 1

real :: mid, mid_new, hi, low

write(*,'(A, 1X)', ADVANCE = 'NO') "Please enter two space separated end points:"
read (*, *) hi, low

mid  = (low + hi)/2.0

eps = 1.0E-07
del = 1

do while (abs(del) > eps) ! assuming that xupper and xlower aren't roots
    
    !print *, 'mid is ', mid
    cmp = f(low)*f(mid)
    if(cmp < 0.0) then
        hi = mid
    else if(cmp > 0.0) then
        low = mid
    else
        !mid = 0 case
        print *, 'Root is ', mid
    end if
    mid_new = (low + hi)/2.0
    !print *, "new mid is", mid_new
    del = (mid_new - mid)/mid
    !print *, "del is ", del
    mid = mid_new
end do
print *, "Root found is ", mid
end program bisection