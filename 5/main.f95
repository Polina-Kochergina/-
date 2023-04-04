program vch5
implicit none
real(8) xx, ll, aa, bb
integer i, j, t, n
real, allocatable :: x(:), f(:), k(:), l(:), a(:), b(:), c(:), d(:), r(:), s(:), h(:)


open(1,file="input")
read(1,*) n
allocate (f(0:n), x(0:n), k(n), l(n), a(n+1), b(n+1), c(n+1), d(n+1), r(2:n), s(2:n), h(n))
do j =0, n-1
read(1,*) x(j), f(j)
enddo

close(1)

c(1)=0
c(n+1)=0
k(1)=0
l(1)=0

do i = 1,n
	h(i) = x(i) - x(i-1)
enddo

  
do i = 2, n
	r(i) = 3*(((f(i) - f(i-1))/h(i) - (f(i-1)-f(i-2))/h(i-1)))
	s(i) = 2*(h(i) + h(i-1))
enddo

do i = 2,n
	k(i) = (r(i) - h(i-1)*k(i-1))/(s(i) + h(i-1)*l(i-1))
	l(i) = -h(i)/(s(i) + h(i-1)*l(i-1))
enddo

do i = n, 2, -1
	c(i) = k(i)-l(i)*c(i+1)
enddo

do i = 1, n
	a(i) = f(i-1)
	b(i) = (f(i)-f(i-1))/h(i) - (c(i+1)+2*c(i))*h(i)/3
	d(i) = (c(i+1) - c(i))/3/h(i)
enddo

aa=minval(x)
bb=maxval(x)

write(*,*) 'введите х от ', aa, ' до ', bb
read(*,*) xx


do i=1, n
if (xx>=x(i-1) .and. (xx<=x(i))) then
	ll = a(i) + b(i)*(xx - x(i-1)) + c(i)*(xx-x(i-1))**2 + d(i)*(xx - x(i-1))**3
	write (*,*) i, 'a(i) = ',a(i), 'b(i) = ', b(i), 'c(i) = ',  c(i),'d(i)=' , d(i)
	t=i
	else
		write (*,*) i, 'a(i) = ',a(i), 'b(i) = ', b(i), 'c(i) = ',  c(i),'d(i)=' , d(i)
endif

enddo

write(*,*) 'f(x) = ', ll, 'x = ' , xx


open(2, file = "output")
write(2,*) ll , 'i      = ', t
write (2,*) 'a(i) = ',a(t), 'b(i) = ', b(t), 'c(i) = ',  c(t),'d(i)=' , d(t)
close(2)


deallocate(f,x,a,b,c,d,k,l,s,r,h)

end program





