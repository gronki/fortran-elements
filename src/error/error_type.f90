module error_type_m

    implicit none
    private

    type, abstract :: status_t
    contains
        procedure(to_str_proto), deferred :: to_str
        procedure :: deal
    end type

    abstract interface
        pure function to_str_proto(self) result(errmsg)
            import :: status_t
            class(status_t), intent(in) :: self
            character(len=:), allocatable :: errmsg
        end function
    end interface

    type, extends(status_t) :: ok_t
    contains
        procedure :: to_str => ok_to_str
    end type

    type, extends(status_t) :: error_t
    contains
        procedure :: to_str => error_to_str
        procedure :: deal => error_deal
    end type

    type, extends(error_t) :: error_message_t
        character(len=:), allocatable :: msg
    contains
        procedure :: to_str => error_message_to_str
    end type

    public :: status_t, ok_t, error_t, error_message_t

contains


    pure subroutine deal(self)
        class(status_t), intent(in) :: self
    end subroutine


    pure subroutine error_deal(self)
        character(len=:), allocatable :: errmsg
        class(error_t), intent(in) :: self

        errmsg = self%to_str()
        error stop "fatal: " // errmsg

    end subroutine

    pure function ok_to_str(self) result(errmsg)
        class(ok_t), intent(in) :: self
        character(len=:), allocatable :: errmsg

        allocate(errmsg, source="ok")

    end function

    pure function error_to_str(self) result(errmsg)
        class(error_t), intent(in) :: self
        character(len=:), allocatable :: errmsg

        allocate(errmsg, source="error")

    end function

    pure function error_message_to_str(self) result(errmsg)
        class(error_message_t), intent(in) :: self
        character(len=:), allocatable :: errmsg

        if (allocated(self % msg)) then
            allocate(errmsg, source=self % msg)
        else
            allocate(errmsg, source="error")
        end if
    end function


end module

