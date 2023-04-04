program vchi7
implicit none
complex s, wn
integer k, j, n, p
complex, parameter :: i = (0, 1)   ! sqrt(-1) 
real(8), parameter :: pi = acos(-1.0)
complex(8), allocatable, dimension(:) :: y, x


open(1, file = 'input1')
read(1,*) n
allocate (x(0:n-1), y(0:n-1))
read(1,*) x
close(1)



write(*,*) x
wn = exp(-i*2*pi/n)					!поворотный множитель

write(*,*) 'ввкдите р: р > 0 сначала ДПФ, р < 0 --- ОДПФ'
read(*,*) p
if (p>0) then

	do k=0, n-1					!дпф
	s=0
		do j=0,n-1
		s = s + x(j)*wn**(k*j)
		enddo
		y(k) = s
	enddo



	open(2, file = "input2")
	write(2,*) n, y
close(2)


	open(2, file = 'input2')
	read(2,*) n, y
		
close(4)


	do k=0, n-1
	s=0
		do j=0,n-1
			s = s + y(j)*wn**(-k*j)
		enddo							!ОДПФ
		x(k)=s/n
	enddo
	write(*,*) x
	open(3, file = "output")
	write(3,*) x
close(3)

	else 
	y = x	
		do k=0, n-1
		s=0
			do j=0,n-1
				s = s + y(j)*wn**(-k*j)
			enddo
			x(k)=s/n					
		enddo

	
		open(2, file = "input2")
		write(2,*) n, x
	
close(2)

		open(2, file = 'input2')
		read(2,*) n, x
close(2)
		do k=0, n-1
		s=0
			do j=0,n-1
			s = s + x(j)*wn**(k*j)
			enddo
			y(k) = s
		enddo


	open(3, file = "output")
	write(3,*) y
close(2)

endif
end program












