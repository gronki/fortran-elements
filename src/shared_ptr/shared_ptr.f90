module shared_ptr_m

    implicit none
    private

    public :: shared_ptr_t

    type :: shared_ptr_t
        ! class(*), pointer :: obj => null()
        integer, pointer, private :: refcounter => null()
    contains
        procedure :: ref_count
        procedure, private :: assignment
        generic :: assignment(=) => assignment
        final :: finalize
    end type

    interface shared_ptr_t
        module procedure :: shared_ptr_ctor
    end interface

contains

    function shared_ptr_ctor() result(self)
        type(shared_ptr_t), allocatable :: self
        ! class(*), intent(in), pointer :: obj
 
        allocate(self)
        ! self % obj => obj
        allocate(self % refcounter, source=1)
    end function

    function ref_count(self)
        class(shared_ptr_t), intent(in) :: self
        integer :: ref_count

        ref_count = self % refcounter
    end function

    elemental impure subroutine assignment(self, other)
        class(shared_ptr_t), intent(inout) :: self
        class(shared_ptr_t), intent(in) :: other

        if (associated(self % refcounter)) then
            self % refcounter = self % refcounter - 1
        end if
        self % refcounter => other % refcounter
        self % refcounter = self % refcounter + 1
        write (*, *) 'assignment, refcount = ', self % ref_count()
    end subroutine

    subroutine finalize(self)
        type(shared_ptr_t), intent(inout) :: self
        self % refcounter = self % refcounter - 1
        write (*, *) 'finalize, refcount = ', self % ref_count()
    end subroutine


end module
