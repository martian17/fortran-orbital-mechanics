        module types
            implicit none
            type body
                character(20)                  :: name
                DOUBLE PRECISION               :: mass
                DOUBLE PRECISION, dimension(3) :: position
                DOUBLE PRECISION, dimension(3) :: velocity
                DOUBLE PRECISION, dimension(3) :: acceleration
            end type
        end module types
        
        subroutine print_body(body1)
            use types
            implicit none
            type (body) :: body1
            !print *, body1%name, body1%position, body1%velocity
            write (*,'(*(G0.17,:,","))')
     >          body1%name,
     >          body1%position,
     >          body1%velocity
        end subroutine
        
        program main
            use types
            implicit none
            integer :: D = 3
            DOUBLE PRECISION :: dt = 1000 ! 1000 second increment
            DOUBLE PRECISION :: time = 0, r, acc, G=6.6743D-11
            integer :: n_steps = 10000, i, j, n_objects, itr ! 1000 steps
            type (body) :: b1, b2
            type (body), dimension(3) :: bodies
            
            bodies(1) = body(
     >          "sun",         ! name
     >          1.989D30,      ! mass
     >          (/ 0D0, 0D0, 0D0 /), ! position
     >          (/ 0D0, 0D0, 0D0 /), ! velocity
     >          (/ 0D0, 0D0, 0D0 /)  ! acceleration
     >      )
            
            bodies(2) = body(
     >          "earth",
     >          5.972D24,
     >          (/ 1.50D11, 0D0,  0D0 /),
     >          (/ 0D0,     30D3, 0D0 /),
     >          (/ 0D0,     0D0,  0D0 /)
     >      )
            
            bodies(3) = body(
     >          "earth2",
     >          5.972D24,
     >          (/ 1.70D11, 0D0,  0D0 /),
     >          (/ 0D0,     30D3, 0D0 /),
     >          (/ 0D0,     0D0,  0D0 /)
     >      )
            
            !
            
            call print_body(bodies(1))
            call print_body(bodies(2))
            
            n_objects = SIZE(bodies) 
            
            
            do itr = 1, n_steps
                time = time + dt;
                write (*,'(*(G0.17,:,":"))') "time", time
                
                do i = 1, n_objects
                    b1 = bodies(i)
                    call print_body(b1)
                    
                    b1%acceleration = 0
                    do j = 1, n_objects
                        ! skip the cycle if i==j
                        if (i .eq. j)cycle
                        b2 = bodies(j)
                        r = norm2(b2%position-b1%position)
                        acc = G*b2%mass/(r*r)
                        b1%acceleration = b1%acceleration
     >                      + (b2%position-b1%position)/r*acc
                    end do
                    bodies(i) = b1
                end do
                
                do i = 1, n_objects
                    b1 = bodies(i)
                    b1%velocity = b1%velocity
     >                  + b1%acceleration*dt
                    b1%position = b1%position
     >                  + b1%velocity*dt
                    bodies(i) = b1
                end do
            end do
            
        end program main