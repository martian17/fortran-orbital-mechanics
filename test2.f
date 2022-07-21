        module test_m
            implicit none
            private
            public test_type
            type test_type
                integer :: i
            contains
                procedure, nopass :: print_hello
                procedure         :: print_int
            end type
        contains
            !> do not process type specific data => nopass
            subroutine print_hello
                print *, "hello"
            end subroutine
        
            !> process type specific data => first argument is "this" of type "class(test_type)"
            !! use class and not type below !!!!
            subroutine print_int(this)
                class(test_type), intent(in) :: this
        
                print *, "i", this%i
            end subroutine
        end module
        
        program main
            use test_m
            implicit none
            type (test_type) :: obj
        
            obj%i = 1
            call obj%print_hello
            call obj%print_int
        end program