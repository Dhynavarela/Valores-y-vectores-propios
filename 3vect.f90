         real :: a(100,100),b(100,100),c(100,100),d(100,100),e(100,100),f(100,100),lambda,y,z,w
         integer p
          n=4
         open(26,file='matrizm.dat',status='replace')
         open(25,file='matri2.dat',status='old')        
         do i=1,4
          read(25,*) (c(i,j), j=1,4)
          write(*,*) (c(i,j), j=1,4)
         enddo
        
         write(*,*) 'Ingresa el valor de lambda a evaluar'
         read(*,*) lambda
         !Matriz identidad    
         DATA (d(1,J),J=1,4)/1,0,0,0/
         DATA (d(2,J),J=1,4)/0,1,0,0/
         DATA (d(3,J),J=1,4)/0,0,1,0/
         DATA (d(4,J),J=1,4)/0,0,0,1/
         print*, 'La nueva matriz con lambda=',lambda,'es:'
         !Se sustituye a lambda en A-lambda*I
         do i=1,4
          do j=1,4            
           c(i,j)=c(i,j)-lambda*d(i,j)
          enddo
          write(26,*)  (c(i,j),j=1,4)
          enddo
          close(26)
          open(28,file='matrizmn.dat',status='replace')
          open(26,file='matrizm.dat',status='old')
         n=4
         do i=1,4
          read(26,*) (a(i,j), j=1,4)
          write(*,*)  (a(i,j), j=1,4)
         enddo
           !INTERCAMBIO FILAS
         do j=1,4
          ai=a(1,j)
          a(1,j)=a(3,j)
          a(3,j)=ai
         enddo
         !RESULTADOS 
         write(*,*) 
         do i=1,4
          write(*,*) (a(i,j), j=1,4)
         enddo
         !INTERCAMBIO FILAS
         do j=1,4
          ai=a(2,j)
          a(2,j)=a(3,j)
          a(3,j)=ai
         enddo
         !RESULTADOS 
         write(*,*) 'nueva matriz'
         do i=1,4
          write(28,*) (a(i,j), j=1,4)
         enddo
         close(28)
         
         open(27,file='matmno.dat',status='replace')
         open(28,file='matrizmn.dat',status='old')
         do i=1,4
          read(28,*) (a(i,j), j=1,4)
          write(*,*)  (a(i,j), j=1,4)
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
         enddo  !imprime una
           do i=1,n
            !print 123,(a(i,j),j=1,n+1)
            write(27,123) (a(i,j),j=1,n)
           enddo 
           print *,
           !enddo  !imprime todas
123       format(5f12.6)
           close(27)
           !saca soluciones
           print*, 'Matriz a resolver'
           open(27,file='matmno.dat',status='old')
           do i=1,4
          read(27,*) (a(i,j), j=1,4)
          write(*,*)  (a(i,j), j=1,4)
         enddo
         m=4
           w=a(m-1,m-1)
           z=-a(m-1,m)
           y=-(z*(a(2,3))+w*(a(2,4)))/a(2,2)
           x=-(y*(a(1,2))+z*(a(1,3))+w*(a(1,4)))/a(1,1)
           write(*,*) 'soluciones'
           print*, x,y,z,w
       end


     
