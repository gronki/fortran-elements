module testobject_m

    use iso_c_binding, only: c_loc, c_ptr
    use iso_fortran_env, only: int64

    implicit none

    type :: testobj_t
        character(len=:), allocatable, private :: secret
    contains
        procedure :: ident
        final :: final
    end type

    interface testobj_t
        module procedure :: testobj_ctor
    end interface

    type, extends(testobj_t) :: testobj_w_assignment_t
    contains
        procedure, private :: assignment
        generic :: assignment(=) => assignment
    end type

    interface testobj_w_assignment_t
        module procedure :: testobj_w_assignment_ctor
    end interface

contains

    function testobj_ctor(secret) result(testobj)
        type(testobj_t) :: testobj
        character(len=*), intent(in) :: secret

        allocate(testobj % secret, source=secret)

        print *, 'constructed ', testobj % ident()
    end function

    function testobj_w_assignment_ctor(secret) result(testobj)
        type(testobj_w_assignment_t) :: testobj
        character(len=*), intent(in) :: secret

        allocate(testobj % secret, source=secret)

        print *, 'constructed ', testobj % ident()
    end function

    function address_str(cptr)
        type(c_ptr), intent(in) :: cptr
        integer(int64) :: intaddr
        character(len=4) :: address_str

        intaddr = transfer(cptr, intaddr)
        intaddr = iand(intaddr, int(z'ffff', kind=kind(intaddr)))
        write(address_str, '(z0.4)') intaddr
    end function

    function ident(self)
        class(testobj_t), intent(in), target :: self
        character(len=:), allocatable :: ident

        select type(self)
          type is (testobj_t)
            ident = 'self=' // address_str(c_loc(self)) &
                // ', buf=' // address_str(c_loc(self % secret))

          type is (testobj_w_assignment_t)
            ident = 'self=' // address_str(c_loc(self)) &
                // ', buf=' // address_str(c_loc(self % secret))

          class default
            error stop
        end select

        if (allocated(self % secret)) then
            if (len_trim(self % secret) > 0) &
                ident = trim(self % secret) // '; ' // ident
        end if
        ident = '[' // ident // ']'
    end function

    subroutine final(self)
        type(testobj_t), intent(in) :: self

        print *, 'finalizing ', self % ident()
    end subroutine

    subroutine assignment(self, other)
        class(testobj_w_assignment_t), intent(inout) :: self
        class(testobj_t), intent(in) :: other

        print *, 'assignment: ', self % ident(), ' <-- ', other % ident()
        if (allocated(self % secret)) deallocate(self % secret)
        if (allocated(other % secret)) allocate(self % secret, source=other % secret)
    end subroutine

end module
