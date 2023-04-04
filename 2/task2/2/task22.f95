program task22 
implicit none 

integer n, k, i, p
real(8) a, b, h, t, im
real(8),parameter :: pi = acos(-1.0) 
real(8), allocatable, dimension(:) :: x, f 

open(1, file = 'input1')
read(1, *) a, b, n, k
write(*,*) a, b, n, k
close(1)

allocate (x(n+1), f(n+1)) 

h = (b-a)/n 

open(2, file = 'input2')
if (k == 1) then
	read(2,"(12x, f8.0)") f(1)
	read(2, *) f(2)
	read(2, *) f(3)
	read(2, *) f(4)
	read(2, *) f(5)
	read(2, *) f(6)
	read(2, *) f(7)
	read(2, *) f(8)
	read(2, *) f(9)
	read(2, *) f(10)
		
else
	read(2,*) f 
	
endif
close(2)

if (k == 1) then				
do i = 1, n+1
		x(i) = (b-a)*cos(pi*(2*i-1)/(2*(n+1)))/2 + (a+b)/2
		
	end do
	x = x(n+1:1:-1)
	write(*,*) 'Рассчитанные узлы:'
	write(*,*)  x	
else					
	do i = 1, n+1 
		x(i) = a + (i-1)*h
	end do

end if  

write(*,*) 'Рассчитанные значения функции:'
write(*,*) f

write(*,*) 'Введите t'
read(*,*) t 

im = 0

do i = 0, n
	im = im + f(i+1) * fi(t, i, x, n)
end do

write(*,*) "f(x)=" , im 

deallocate (x, f)

contains

function fi(t, p, x, n)					

	integer i, p, n
	real(8) t, fi, x(0:n)
	
	fi = 1
	
	do i=0, p-1
		fi = fi * (t - x(i)) / (x(p) - x(i))
	end do
	
	do i=p+1, n
		fi = fi * (t - x(i)) / (x(p) - x(i))
	end do
	
end function fi

end program 





