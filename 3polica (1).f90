  !Este programa calcula el polinomio característico
          real :: a(4,4),p(5),s,B(1:4,1:4),C(1:4,1:4)
         open(25,file='matri2.dat',status='old')
         do i=1,4
          read(25,*) (a(i,j), j=1,4)
          write(*,*) (a(i,j), j=1,4)
         enddo
         n=4
         print*, 'La traza es', TRM(n,A)
          
         if (MOD(n,2).ne.0)  then   !n es par
          P(1)=-1.d0
         else 
          P(1)= 1.d0
        endif
        print*, P(1)
        do l=1, n
         if (l.eq.1) then
	      C = A
         else
          do i=1, n
           do j=1, n
            s=0.0
             do k=1, n
              s=s+B(i,k)*A(k,j)
             end do
             C(i,j)=s
           enddo
          enddo
         endif
         t0=TRM(n,C)/l
         P(l+1)=-t0*P(1)
         if (l<n) then
          do i=1, n
           do j=1, n
            if (j.eq.i) then 
		     B(i,j)=C(i,j)-t0
            else 
	         B(i,j)=C(i,j)
            endif
           enddo
          enddo
         endif
        enddo    
         print *,' '
         print *,' Polinomio caracteristico P(l): '
         print*,'      Grado',       '          ' ,'Valor'
         do k=1, n+1
          write(*,*) n-k+1, P(k)
         enddo
         print *,' ' 
         end
   
         !calcula la traza de la matriz A(n,n)
         real Function TRM(n, A)
         integer n,i
         real t0, A(1:n,1:n)
         t0=0.0
         do i=1, n
          t0=t0+A(i,i)
         end do
         TRM=t0        
         end




