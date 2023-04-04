program odin
implicit none

integer n, i, j, k
real s, x, L
real, allocatable, dimension(:) :: ff, xx

open(1, file = 'input1')
read(1,*) n
allocate (xx(n))

do i = 0, n-1	
	read(1,*) xx(i) 
end do
close(1)



allocate (ff(n))
open(2, file = 'input2')
do i = 0, n-1	
	read(2,*) ff(i) 
end do
close(2)

read(*,*) x

L = ff(0)
do j=1, n-1
	L = L + fun_f(0, j, ff, xx, n) * fun_w(j-1, xx, x, n)
end do

print *, ff(0)
Write(*,*) 'f(x) = ', L, 'x = ', x

contains

function fun_f(q, p, ff, xx, n)
	
	integer q, p, i, l, n
!	real, dimension(n) ff, xx
	real prod, fun_f, ff(n), xx(n)
	
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


function fun_w(k, xx, x, n) result(p)

	real p, x, xx(n)
	integer i, k, n, l
!	real, dimension(n) xx

	p = 1
	do i=0, k
		p = p * (x - xx(l))
	end do

end function fun_w


end program
