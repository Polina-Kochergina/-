program vchi_3
implicit none
integer i, j, n, k, no
real(8) max, aa, bb, sum
real(8), allocatable :: A(:,:), X(:), ai(:), b(:), D(:,:)

open(1,file="input")
read(1,*) n

allocate (A(n,n), X(n), D(n,n+1), b(n))
do j =1, n
read(1,*) A(j,:), b(j)
enddo
close(1)


D(1:n,:) = A					!A - матрица коэффициентов
D(:,n+1) = b					!В - вектор значений			Ах=b, вектор х нужно найти

do i = 1,n
	if (a(i,i) == 0) then
	write(*,*) "net resheniy"		!проверим что решение существует
	stop
	endif
enddo

do i=1, n-1					!отсортируем, чтобы избежать деления на очень маленькое число
	max=abs(D(i,i))
	no = i
	do j = i+1,n
		if (abs(D(j,i))>max) then 
		max=abs(D(j,i))
		no =  j
		endif
	end do
	ai=D(i,:)
	D(i,:) = D(no,:)
	D(no,:)=ai
	
enddo


do k = 1, n					!прямой ход
	D(k,:)=D(k,:)/d(k,k)
	do i = k+1, n
		bb = d(i,k)
		do j = k, n+1
			D(i,J)=d(i,j) - d(k,j)*bb
		enddo
	enddo
enddo

	
sum=0
do i=n,1,-1					!обратный ход
  x(i)=d(i,n+1)/d(i,i)
      do j=n,i,-1
      sum=d(i-1,j)*x(j)+sum
      enddo
  d(i-1,n+1)=d(i-1,n+1)-sum
  sum=0
enddo

!open(2,file = "output")		
write(*,*) x					
close(2)
	
end program	
		
	
		
