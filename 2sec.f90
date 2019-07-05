         !Este programa utiliza el método de la secante para encontrar los valores propios
         dimension x(0:100)
         !El método de la bisección indica entre que valores de x0 y x1 se encuentra la raiz
          a=-10
          b=10
          n=1000
          h=(b-a)/n
           do i=1,n
              xi=a+i*h
              xi1=xi+h
              fi=f(xi)
              fi1=f(xi1)
              p=fi*fi1
           if(p.lt.0.) print *, 'entre', xi, 'y' , xi1, 'hay una raíz'
          enddo
        !Comienza el método de la scante
         write(*,*) 'Escribe los valores de x0 y x1:'
         read(*,*) x(0),x(1)
         valor= abs(f(x1))
         k=1
          do while (valor.gt.1E-5)
            x(k+1)=x(k)-(f(x(k))*(x(k)-x(k-1))/(f(x(k))-f(x(k-1))))
                  valor=abs(f(x(k+1)))
                   k=k+1
          enddo
          write (*,*) 'La raíz es:',x(k)
         end

!          Aquí se define a la función

          real function f(x)
               f=((-x**3)+(3.*x**2)+(6.*x)-(8.))
        end 
                
