module mod_fun2_1
implicit none


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


function fun_w(k, xx, x, n)
	real p, x, xx(n), fun_w
	integer i, k, n, l
!	real, dimension(n) xx

	fun_w = 1
	do i=0, k
		fun_w = fun_w * (x - xx(i))
	end do

end function fun_w


end module mod_fun2_1
