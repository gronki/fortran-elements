
module error_m

    use error_type_m
    implicit none
    private


    type :: error_handler_t
        class(status_t), allocatable :: status
    contains
        procedure, pass(handler) :: set => set_handler_status
        procedure :: caught
        procedure :: move_status
        procedure :: pop => pop_status
        procedure :: get => get_status
        procedure :: to_str
    end type

    interface raise
        module procedure :: set_handler_status
        module procedure :: set_handler_status_message
    end interface

    character(len=*), parameter :: newline_char = char(10)

    public :: error_handler_t, get_status, pop_status, raise, caught

contains

    pure function caught(handler)
        class(error_handler_t), intent(in), optional :: handler
        logical :: caught

        if (present(handler)) then
            select type(status => handler % status)
              class is (error_t)
                caught = .true.
                return
            end select
        end if

        caught = .false.

    end function

    pure subroutine move_status(handler, status)
        class(error_handler_t), intent(inout) :: handler
        class(status_t), intent(inout), allocatable, optional :: status

        if (allocated(handler % status)) then
            if (present(status)) then
                call move_alloc(handler % status, status)
            else
                deallocate(handler % status)
            end if
        else
            allocate(status, source=ok_t())
        end if
    end subroutine


    impure function pop_status(handler) result(status)
        class(error_handler_t), intent(inout), optional :: handler
        class(status_t), allocatable :: status

        if (present(handler)) then
            call handler % move_status(status)
            return
        end if

        allocate(status, source=ok_t())

    end function


    pure function get_status(handler) result(status)
        class(error_handler_t), intent(in), optional :: handler
        class(status_t), allocatable :: status

        if (present(handler)) then
            if (allocated(handler % status)) then
                allocate(status, source=handler % status)
                return
            end if
        end if

        allocate(status, source=ok_t())

    end function


    pure subroutine set_handler_status(status, handler)
        class(error_handler_t), intent(inout), optional :: handler
        class(status_t), intent(in) :: status

        if (.not. present(handler)) then
            call status % deal
            return
        end if

        if (allocated(handler % status)) then
            call handler % status % deal
            deallocate(handler % status)
        end if

        allocate(handler % status, source=status)

    end subroutine


    pure subroutine set_handler_status_message(msg, handler)
        character(len=*), intent(in) :: msg
        class(error_handler_t), intent(inout), optional :: handler

        call set_handler_status(error_message_t(msg), handler)
    end subroutine

    pure function to_str(handler) result(errmsg)
        class(error_handler_t), intent(in) :: handler
        character(len=:), allocatable :: errmsg
        class(status_t), allocatable :: status

        allocate(status, source=handler % get())
        allocate(errmsg, source=status % to_str())

    end function

end module

