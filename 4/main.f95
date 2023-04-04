program vch4
implicit none
real(8) eps, err, max, s1, s2
integer c, i, j, k, n, no
real, allocatable :: A(:,:), At(:,:), B(:), Atb(:), P(:), Q(:), x(:), x0(:), AtA(:,:), D(:,:), ai(:)


open(1,file="input")
read(1,*) n
allocate(A(n,n), B(n), At(n,n), AtA(n,n), AtB(n), P(n), Q(n), x(n), x0(n), D(n,n+1), ai(n))
do j =1, n
read(1,*) A(j,:), b(j)
enddo


 c=0
do i = 1, n
s1=0
s2=0
	do j = 1, n
	if (j == i) then 
		s1 = a(i,i)
	else 
		s2 = s2 + a(i,j)
	endif
	enddo
	
	if(s2>s1) then
		write(*,*) 'не удовлетворяет условию диагонального преоблодания',i, 'строка'
	else 
		c=c+1
	endif
enddo


if (c==n) then
	write(*,*) 'ok! удовлетворяет условию диагонального преоблодания'
else
	write(*,*) ' Continue? 1 -- yes or 0 -- no'
	read(*,*) err
	if (err == 0) then
		stop
	endif
endif



At = transpose(A)
do i = 1, n
	do k = 1, n
	AtA(i,k) = 0
		do j = 1, n
		AtA(i,k) = AtA(i,k)+(At(i,j)*A(j,k))
		enddo
	enddo
enddo

do i = 1, n
	atb(i)=0
	do j = 1, n
	AtB(i) = atb(i) + at(i,j)*b(j)
	enddo
enddo


D(1:n,:) = AtA					
D(:,n+1) = AtB	

do i=1, n-1					
	max=abs(D(i,i))
	no = i
	do j = i+1,n
		if (abs(D(j,i))>max) then 
		max=D(j,i)
		no =  j
		endif
	end do
	ai=D(i,:)
	D(i,:) = D(no,:)
	D(no,:)=ai
	
	do j=i+1,n
  		D(j,:)=D(j,:) - D(i,:)*D(j,i)/D(i,i)
    	enddo
	
enddo


x0(1:n) = 0
x(1:n) = 1

eps = 0.0001
k=0
do while (maxval(abs(x-x0))>eps)
k=k+1
	do i = 1, n
	x0(i)=x(i)
	x(i)=0
	Q(i)=D(i,n+1)/D(i,i)
	P(:)=-D(i,:)/ata(i,i)
	P(i)=0
 		do j = 1, n
			x(i)=x(i)+p(j)*x0(j)+q(i)/n
		enddo
	enddo

if (k == 50) then
	exit
endif

enddo
write(*,*) 'k =', k, 'eps =', eps

print *, x
open(2, file ='output')
write(2,*) 'k =', k, 'eps =', eps
write(2,*) x
close(2)

deallocate(A, B, At, AtA, AtB, P, Q, x, D, ai, x0)
end program
