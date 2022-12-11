module str_hash_base_m

    implicit none
    integer, parameter :: int_hash_k = selected_int_kind(5)

    type, abstract :: hash_algorithm_t
    contains
        procedure(hash_key_proto), pass(hasher), deferred :: hash
    end type

    abstract interface
        elemental function hash_key_proto(hasher, key) result(hash)
            import :: hash_algorithm_t, int_hash_k
            class(hash_algorithm_t), intent(in) :: hasher
            character(len=*), intent(in) :: key
            integer(kind=int_hash_k) :: hash
        end function
    end interface

end module


module stupid_hash_m
    use str_hash_base_m
    implicit none

    type, extends(hash_algorithm_t) :: stupid_hash_t
        integer(kind=int_hash_k) :: modulo = 64
    contains
        procedure, pass(hasher) :: hash => stupid_hash
    end type

contains

    elemental function stupid_hash(hasher, key) result(hash)
        class(stupid_hash_t), intent(in) :: hasher
        character(len=*), intent(in) :: key
        integer(kind=int_hash_k) :: hash

        integer :: i

        hash = 0
        do i = 1, len(key)
            hash = mod(hash + ichar(key(i:i)), hasher % modulo)
        end do
    end function

end module


module str_hash_m

    use stupid_hash_m

    implicit none

    type, extends(stupid_hash_t) :: default_hash_t
    end type

end module
