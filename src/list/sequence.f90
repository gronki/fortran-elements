module sequence_m

    implicit none
    private

    public :: sequence_t, sequence_item_t, size

    type, abstract :: sequence_t
    contains
        procedure(ptr_at_proto), deferred :: ptr_at
        procedure(getcopy_at_proto), deferred :: getcopy_at
        procedure :: at

        procedure(length_proto), deferred :: length
    end type

    type :: sequence_item_t
    contains
    end type

    abstract interface
        pure subroutine ptr_at_proto(self, idx, ptr)
            import :: sequence_t
            class(sequence_t), intent(inout), target :: self
            integer, intent(in) :: idx
            class(*), pointer, intent(inout) :: ptr
        end subroutine

        pure subroutine getcopy_at_proto(self, idx, dest)
            import :: sequence_t
            class(sequence_t), intent(in) :: self
            integer, intent(in) :: idx
            class(*), allocatable, intent(inout) :: dest
        end subroutine

        pure function length_proto(self)
            import :: sequence_t
            class(sequence_t), intent(in) :: self
            integer :: length_proto
        end function
    end interface

    interface size
        module procedure :: sequence_size_alias
    end interface

contains

    function at(self, idx)
        class(sequence_t), target :: self
        integer, intent(in) :: idx
        class(*), pointer :: at

        call self % ptr_at(idx, at)
    end function


    pure function sequence_size_alias(self) result(list_size)
        class(sequence_t), intent(in) :: self
        integer :: list_size

        list_size = self % length()
    end function

end module
