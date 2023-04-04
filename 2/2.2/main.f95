program dva
use mod_interpol				! присоденяем модуль с функцией рассчитывающей фи
implicit none

integer n, i, j, k, m
real a, b, s, x, L, PI
real, allocatable, dimension(:) :: f, xx

open(1, file = 'input1')
read(1, *) a, b, n, k
write(*,*) a, b, n, k
close(1)
! входное n -- это количество пробелов

m = n + 1

PI = acos(-1.0)

allocate (xx(m))
if (k == 0) then				! равномерная сетка

	do i = 1, m
		xx(i) = a + (i-1)*(b-a)/n 
		print *, xx(i)
	end do
	print *, xx
else						! чебышевская сетка
	
	do i = 1, m
		!xx(i) = -cos((2*i -1)*acos(-1.0)/(2*n+2))
		xx(i) = (a+b)/2 - (b-a)*cos(PI*(2*i-1)/(2*m))/2
		print *, xx(i)
	end do
	print *, xx
	!s = (b - a)/n
	!do i = 0, n-1
	!	xx(i) = xx(i) + a +1		! масштабируем
	!	xx(i) = (xx(i) - a)*s + a
	!enddo
end if
		
		
allocate (f(m))
open(2, file = 'input2_cheb')
	do i = 1, m	
		read(2,*) f(i) 
	end do
close(2)
print *, f

read(*,*) x

L = 0
do i=0, n-1
	L = L + f(i) * phi(x, i, xx, n)
end do

write(*,*) "f(x)=" , L, "x =",  x

end program dva


