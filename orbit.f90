        !subroutine dist(v1, v2, n)
        !    DOUBLE PRECISION, dimension(*) :: v1
        !    DOUBLE PRECISION, dimension(*) :: v2
        !    DOUBLE PRECISION,              :: sum
        !    
        !    do i = 1, n
        !        sum = sum + (v2(i)-v1(i))**2
        !    end do
        !end subroutine
        
        !type body
        !    DOUBLE PRECISION, dimension(3) :: position
        !    DOUBLE PRECISION, dimension(3) :: velocity
        !    DOUBLE PRECISION, dimension(3) :: acceleration
        !    DOUBLE PRECISION               :: mass
        !    character       , dimension(20):: name
        !end type
        
        !program main
        !    implicit none
        !    integer :: D = 3
        !    
        !    type (body) :: sun
        !    sun%position     = (/ 0, 0, 0 /)
        !    sun%velocity     = (/ 0, 0, 0 /)
        !    sun%acceleration = (/ 0, 0, 0 /)
        !    sun%mass         = 1.989D30
        !    print *, sun%mass
        !    
        !end program main
        
        type fruit
            real      :: diameter  ! in mm
            real      :: length    ! in mm
            character :: colour
        end type fruit
        
        program main
            implicit none
            type (fruit) :: apple, banana
            apple = fruit(50, 45, "red")
            banana%diameter = 40
            banana%length   = 200
            banana%colour   = "yellow"
            print *, banana%length
        end program main