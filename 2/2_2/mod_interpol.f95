module mod_interpol
implicit none

real(8), parameter :: PI = acos(-1.0)

contains

function phi(x, j, aa, n)					! запишем функцию фи для вычисления методом Лагранжа

	real(8) phi, x
	real(8) aa(0:n)
	integer i, j, n
	
	phi = 1
	
	do i=0, j-1
		phi = phi * (x - aa(i)) / (aa(j) - aa(i))
	end do
	
	do i=j+1, n
		phi = phi * (x - aa(i)) / (aa(j) - aa(i))
	end do
	
end function phi

end module


