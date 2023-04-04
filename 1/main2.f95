program main1
implicit none
integer m, n, i, j, f, k
integer, allocatable, dimension(:,:) :: A, B, C
open(1, file = 'input2.dat')
read(1, *) n, m
write(*,*) n, m

allocate (A(m, n))
do i = 1, m
	read(1,*) A(i,:) 
enddo

allocate (B(n, m))
do i = 1, n
	read(1,*) B(i,:) 
enddo
close(1)

allocate (C(m,m))
do i = 1, m
	do k = 1, i
	C(i,k) = 0
		do j = 1, m
		C(i,k) = C(i,k)+(A(i,j)*B(j,k))
		enddo
	enddo
enddo

write(*,*) C
open (2, file = 'res2')
do i = 1, m
write(2, *) C(i,:)
enddo
close(2)
deallocate (A)
deallocate (B)
deallocate (C)
end program
