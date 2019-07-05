         real :: a(100,100),b(100,100),c(100,100),d(100,100),e(100,100),f(100,100),lambda
         integer p
         open(26,file='matrizl.dat',status='replace')
         open(25,file='matri.dat',status='old')        
         do i=1,3
          read(25,*) (c(i,j), j=1,3)
          write(*,*) (c(i,j), j=1,3)
         enddo
         n=3
         write(*,*) 'Ingresa el valor de lambda a evaluar'
         read(*,*) lambda
         !Matriz identidad    
         DATA (d(1,J),J=1,3)/1,0,0/
         DATA (d(2,J),J=1,3)/0,1,0/
         DATA (d(3,J),J=1,3)/0,0,1/
         print*, 'La nueva matriz con lambda=',lambda,'es:'
         !Se sustituye a lambda en A-lambda*I
         do i=1,3
          do j=1,3            
           c(i,j)=c(i,j)-lambda*d(i,j)
          enddo
          write(26,*)  (c(i,j),j=1,3)
          enddo
          close(26)
          
         open(27,file='matn.dat',status='replace')
         open(26,file='matrizl.dat',status='old')
         n=3
         write(*,*) 
         do i=1,3
          read(26,*) (a(i,j), j=1,3)
          write(*,*)  (a(i,j), j=1,3)
         enddo
          do k=1,n
           do i=k+1,n
            do j=k+1,n+1
             b(i,j)=(a(k,j)/a(k,k))*a(i,k)
             a(i,j)=a(i,j)-b(i,j)
            enddo
           enddo
          do i= k+1, n
           a(i,k)=0
          enddo 
           do i=1,n
            write (27,123)(a(i,j),j=1,n)
           enddo 
           enddo  
123       format(5f12.6) 
           close(27)
          !hace determinante
          det=1.
           do i=1,n
            det=det*a(i,i)
           enddo
           print *, 'Determinate:', det
           !saca soluciones
           print*, 'Matriz a resolver'
           open(27,file='matn.dat',status='old')
           do i=1,2
          read(27,*) (a(i,j), j=1,3)
          write(*,*)  (a(i,j), j=1,3)
         enddo
         m=2    
           do j=1,m+1
            c(1,j)=a(1,j) 
           enddo
           divisor= a(1,1)
           !print *,'se divide por', divisor
           do j=1,m+1
            a(1,j)=((c(1,j))/divisor)
           enddo
           write(*,*)
           do i=1,m
 457           print 123,(a(i,j),j=1,m+1)
           enddo 
           do j=1,m+1
            c(m,j)=a(m,j) 
           enddo
           divisor= a(m,m)
           if(divisor.eq.0) stop 
           !print *, 'se divide por', divisor
           do j=1,m+1
            a(m,j)=((c(m,j))/divisor)
           enddo
           do i=1,m
            !print 123,(a(i,j),j=1,m+1)
           enddo
           do j=1,m+1
            a(1,j)=a(1,j)-a(2,j)         
           enddo
           print*,
           do i=1,m
            print 123,(a(i,j),j=1,m+1)
           enddo
           z=a(m,m)
           y=-a(m,m+1)
           x=-(z*(a(1,3))+y*(a(1,2)))
           write(*,*) 'soluciones'
           print*, x,y,z
          end
          
 
         
          
          
