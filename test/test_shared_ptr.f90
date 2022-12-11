program test_shared_ptr

    use shared_ptr_m

    type :: ptr_wrap
        type(shared_ptr_t) :: ptr
    end type

    block
        type(shared_ptr_t) :: ptr1, ptr2, ptr3
        type(ptr_wrap) :: wrap1, wrap2

        ptr1 = shared_ptr_t()

        wrap1 % ptr = ptr1
        wrap2 = wrap1

        ptr2 = ptr1

        block
            ptr3 = ptr2
            ptr2 = ptr3
        end block

    end block

end program
