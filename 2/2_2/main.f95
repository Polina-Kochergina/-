program dva
use mod_interpol				! присоденяем модуль с функцией рассчитывающей фи
implicit none

integer n, i, j, k, m
real(8) a, b, s, x, L, q,w,e,r,t,y
real(8), allocatable, dimension(:) :: f, xx

open(1, file = 'input1')
read(1, *) a, b, n, k
write(*,*) a, b, n, k
close(1)
! входное n -- это количество отрезков, на которые разбивается [a,b]

m = n + 1	! m -- количество узлов


allocate (xx(m))
if (k == 0) then				! равномерная сетка

	do i = 1, m
		xx(i) = a + (i-1)*(b-a)/n 
	end do


else						! чебышевская сетка
	
	do i = 1, m
		
		! xx(i) = cos(PI*(2*i-1)/(2*m))			!корни полинома чебышева 1го рода на отрезке [-1,1]
		xx(i) = (a+b)/2 + (b-a)*cos(PI*(2*i-1)/(2*m))/2 	!корни полинома чебышева 1го рода на отрезке [a,b]
		! print *, xx(i)
	end do
	xx = xx(m:1:-1)				!реверс массива


end if
		
		
allocate (f(m))
open(2, file = 'input2')
if (k /= 0) then
	read (2, *) q, w, e, r, t, y
endif

	do i = 1, m	
		read(2,*) f(i) 
	end do
close(2)

read(*,*) x

L = 0
do i=0, m-1
	L = L + f(i+1) * phi(x, i, xx, n)
end do
write(*,*) "узлы чебышевской сетки:", xx
write(*,*) "f(x)=" , L, "x =",  x

deallocate (xx, f)
end program dva


