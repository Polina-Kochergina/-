program task21
implicit none

integer n, i
real  t, im
real, allocatable, dimension(:) :: x, f

open(1, file = 'input1')
read(1,*) n
allocate (x(n))
read(1,*) x
close(1)


open(2, file = 'input2')
allocate (f(n))
read(2,*) f
close(2)

write(*,*) 'Введите t из промежутка от', minval(x), 'до', maxval(x) 
read(*,*) t


im = f(0)

do i=1, n-1
	im = im + r(0, i, n, x, f) * w(i-1, n, t, x)
end do

write(*,*) 'f(t) = ', im 

open(3,file="output")
write(3,*) im
close(3)

deallocate(x, f) 

contains

function r(q, p, n, x, f)
	
	integer   q, p, k, j, n 
	real r, pp, x(n), f(n)
	
	r = 0
	
	do j=q, p
		pp = 1
		do k=q, j-1
			pp = pp * (x(j) - x(k))
		end do
		do k=j+1, p
			pp = pp * (x(j) - x(k))
		end do
		r = r + f(j) / pp
		
	end do
end function r


function w(l, n, t, x)
	real t, x(n), w
	integer i, l, n 
	w = 1
	do i=0, l
		w = w * (t - x(i))
	end do

end function w


end program
