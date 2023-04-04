program vchi6
use mod_fun2_1
implicit none

integer n, i, j, k
real(8) s, L1, L2, L3, h, f1, f2, x1, x2, x3
real(8), allocatable, dimension(:) :: ff, xx

open(1, file = 'input')
read(1,*) n
allocate (xx(n), ff(n))

	read(1,*) xx(0:n-1)
	read(1,*) ff(0:n-1)

close(1)
Write(*,*) xx
Write(*,*) ff

read(*,*) x2

h=0.001
x1 = x2 - h
x3 = x2 + h

Write(*,*) x1
Write(*,*) x3

L1 = ff(0)
L2 = ff(0)
L3 = ff(0)

Write(*,*) l1, l2, l3

do j=1, n-1
	L1 = L1 + fun_f(0, j, ff, xx, n) * fun_w(j-1, xx, x1, n)
	L2 = L2 + fun_f(0, j, ff, xx, n) * fun_w(j-1, xx, x2, n)
	L3 = L3 + fun_f(0, j, ff, xx, n) * fun_w(j-1, xx, x3, n)
end do

Write(*,*) 'f(x) = ', L2, 'x = ', x2


f1 = (l3 - l1)/2/h
f2 = (l3 - 2*l2 + l1)/h**2


open(1, file = 'output')
write(1,*) f1, f2
write(1,*) ' первая и вторая производная f(x) в точке', x2
end program
