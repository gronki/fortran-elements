

module str_dict_m

    use str_hash_m

    implicit none


    type :: dict_key_t
        character(len=:), allocatable :: key
    end type

    type :: dict_item_t
        type(dict_key_t) :: key
        class(*), allocatable :: value
    end type

    type :: dict_item_bucket_t
        integer(kind=int_hash_k) :: hash
        class(dict_item_t), allocatable :: items(:)
    end type

    type :: str_dict_t
        class(hash_algorithm_t), allocatable :: hasher
        type(dict_item_bucket_t), allocatable, private :: buckets(:)
        integer(kind=int_hash_k), allocatable, private :: hashes(:)
    end type

contains

    elemental function hash_dict_key(hasher, dict_key) result(hash)
        class(hash_algorithm_t), intent(in) :: hasher
        type(dict_key_t), intent(in) :: dict_key
        integer(kind=int_hash_k) :: hash

        hash = hasher % hash(dict_key % key)
    end function

end module
