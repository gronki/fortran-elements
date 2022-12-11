program list_test

    use list_m
    ! use list_tools_m

    implicit none

    type(list_t) :: list1, list2, list3
    integer :: i, temp


    list1 = list_t([(i, i = 100, 105)])
    list2 = list_t([(i, i = 200, 205)])
    list3 = list_t([(i, i = 300, 305)])
    print *, size(list1), size(list2), size(list3)

    print *, 'list1 % extend(list2)'
    call list1 % extend(list2)
    print *, size(list1), size(list2), size(list3)

    print *, 'list1 % extend(temp(list3))'
    call list1 % extend(detach(list3))
    print *, size(list1), size(list2), size(list3)

    call print_vect(list1)
    call print_vect(list2)
    call print_vect(list3)


    list3 = slice(slice(list1, stride=-2), stride=-2)
    list3 = list3 + [3,4]
    list3 = detach(list3) + [5,6]
    ! TODO: assignment <list_t> = <templist_t>
    ! TODO: assignment <list_t> = <list_view_t>

    call print_vect(list3)
    ! print *, as_int(list3 % view())
    call print_vect(list_t() + detach(list_t([1,2,3])) + list3 % slice(stride=-2) + detach(list_t([4,5,6])))
    call print_vect(list3 + slice(list3 % detach(), stride=-1))
    call print_vect(list_t() + [1,2,3] + [7,8,9])
    ! call print_vect(list_t(slice(list_t([(i, i = 777, 888)]), stride=2)))



contains

    subroutine print_vect(vect)
        use sequence_m
        class(sequence_t) :: vect
        integer :: i

        do i = 1, vect % length()
            select type (val => vect % at(i))
              type is (integer)
                write(*, '(i0,a)', advance='no') val, ' '
            end select
        end do

        write (*,*)
    end subroutine

    pure function even_int(x)
        class(*), intent(in) :: x
        logical :: even_int

        even_int = .false.

        select type(x)
          type is (integer)
            even_int = mod(x, 2) == 0
        end select
    end function

    ! elemental function as_int(item) result(retval)
    !     class(view_item_t), intent(in) :: item
    !     integer :: retval

    !     select type (item_value => item % value)
    !       type is (integer)
    !         retval = item_value
    !       class default
    !         error stop
    !     end select
    ! end function
end program
