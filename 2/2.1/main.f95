program odin
implicit none

integer n, i, j, k
real t, im
real, allocatable, dimension(:) :: f, x

open(1, file = 'input1')
read(1,*) n
allocate (x(n))
read(1,*) x

close(1)

write(*,*) x

	

allocate (f(n))
open(2, file = 'input2')
read(2,*) f

close(2)

write(*,*) f

read(*,*) t

L = ff(0)

do j=1, n-1
	im = im + fun_f(0, j, f, x, n) * fun_w(j-1, x, t, n)
end do

Write(*,*) 'f(t) = ', im, 't = ', t


contains

function fun_f(q, p, f, x, n)
	
	integer q, p, i, l, n
	real prod, fun_f, f(n), x(n)
	
	fun_f = 0
	
	do i=q, p
		prod = 1
		do l=q, i-1
			prod = prod * (xx(i) - xx(l))
			
		end do
		do l=i+1, p
			prod = prod * (xx(i) - xx(l))
		end do
		
		fun_f = fun_f + ff(i) / prod
		
	end do
	

end function fun_f


function fun_w(k, x, t, n)
	real p, x, xx(n), fun_w
	integer i, k, n, l

	fun_w = 1
	do i=0, k
		fun_w = fun_w * (x - xx(i))
	end do

end function fun_w


end program
