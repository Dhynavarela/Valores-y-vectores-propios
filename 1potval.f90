         real:: a(4,4),x(4),y(4),z(4),ab(4),lam,di,dif,g,h,b
         real p
         tol=1.17E-38
         open(1,file='matriz.dat',status='old')
         do i=1,4
          read(1,*) (a(i,j), j=1,4)
          write(*,*) (a(i,j), j=1,4)
         enddo

         write (*,*) 'Componentes de x'
         do i=1,4
          write(*,*) i
          read(*,*) x(i)
         enddo

         write(*,*)'Resultado '
         do i=1,3
          y(i)=dot_product (a(i,1:4),x(1:4))
          write (*,*) y(i)
         enddo
     
         factor=maxval(abs(y))       
         p=maxloc(abs(y),1)
         print*, 'El maximo valor absoluto es', factor,'en poscion', p
         yp=y(int(p))
         !print *, 'El valor real es', yp
         x=y*(1./yp)
         !write(*,*)'El resultado es', 
         g=factor
         print*,

         write(*,*)'El nuevo valor propio es', factor
         write(*,*) 'El nuevo vector es'
         do i=1,4
          z(i)=y(i)/factor
          write(*,*) z(i)
         enddo
         !SE REPITE
         print*,
         print*,
         write(*,*)'Resultado con el vector anterior'
         do i=1,4
          y(i)=dot_product (a(i,1:4),z(1:4))
          write (*,*) y(i)
         enddo
     
         factor=maxval(abs(y))       
         p=maxloc(abs(y),1)
         print*, 'El maximo valor absoluto ahora es', factor,'en poscion', p
         yp1=y(int(p))
         !print *, 'El valor real es', yp1
         x=y*(1./yp1)
         !write(*,*)'El resultado es', x
          h=factor
         print*,
         write(*,*)'El nuevo valor propio es', factor
         write(*,*) 'El nuevo vector es'
         do i=1,4
          z(i)=y(i)/factor
          write(*,*) z(i)
         enddo
         
         dif=h-b
         di=abs(dif)
         
         do while(di.gt.tol)
         print*,
         print*,
         write(*,*)'Resultado con el vector anterior'
         do i=1,4
          y(i)=dot_product (a(i,1:4),z(1:4))		
          write (*,*) y(i)
         enddo
		
		 h=factor	
         factor=maxval(abs(y))       
         p=maxloc(abs(y),1)
         print*, 'El maximo valor absoluto ahora es', factor,'en poscion', p
         yp1=y(int(p))
         !print *, 'El valor real es', yp1
         x=y*(1./yp1)
         !write(*,*)'El resultado es', x
          b=factor
         print*,
         write(*,*)'El nuevo valor propio es', factor
         write(*,*) 'El nuevo vector es'
         do i=1,4
          z(i)=y(i)/factor
          write(*,*) z(i)
         enddo
         dif=h-b
         di=abs(dif)
         enddo

         end
