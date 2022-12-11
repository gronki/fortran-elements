module list_m

    use sequence_m

    implicit none
    private

    public :: list_t, list_item_t, size, detach
    public :: slice, pack, filter, subset, operator(+)

    type, extends(sequence_item_t) :: list_item_t
        class(*), allocatable :: value
    end type

    type, extends(sequence_item_t) :: view_item_t
        class(*), pointer :: value
    end type

    type, extends(sequence_t) :: list_t
        private
        type(list_item_t), allocatable :: items(:)
        integer :: num_items = 0
    contains
        procedure, public :: ptr_at, getcopy_at, item_at, length, remove
        procedure :: extend_array, extend_list
        generic, public :: extend => extend_array, extend_list
        procedure, public :: insert, insert_item
        procedure :: adjust, realloc_items, insert_empty
        procedure, public :: detach => templist_from_list
        procedure, public :: slice => slice_list, pack => pack_list, &
            filter => filter_list, subset => subset_list
        procedure :: assign_list
        generic :: assignment(=) => assign_list
        procedure, public :: erase
    end type

    type, extends(list_t) :: templist_t
    end type

    interface list_t
        module procedure :: list_from_array
        module procedure :: list_from_sequence
        module procedure :: list_empty
    end interface

    interface detach
        module procedure :: templist_from_list
    end interface

    integer, parameter :: min_capacity = 4

    interface pack
        module procedure :: pack_list
    end interface

    interface filter
        module procedure :: filter_list
    end interface

    interface slice
        module procedure :: slice_list
    end interface

    interface subset
        module procedure :: subset_list
    end interface

    interface operator(+)
        module procedure :: concat_list_list
        module procedure :: concat_list_arr
        module procedure :: concat_arr_list
    end interface

    interface
        pure function filter_proto(x)
            class(*), intent(in) :: x
            logical :: filter_proto
        end function
    end interface

contains

    pure function list_from_sequence(sequence) result(newlist)
        class(sequence_t), intent(in) :: sequence
        type(list_t) :: newlist

        call newlist % extend(sequence)

    end function


    pure function list_from_array(arr) result(newlist)
        class(*), intent(in) :: arr(:)
        type(list_t) :: newlist

        call newlist % extend(arr)

    end function


    pure function list_empty() result(newlist)
        type(list_t) :: newlist
    end function


    function templist_from_list(list) result(newlist)
        class(list_t) :: list
        type(templist_t) :: newlist

        call move_alloc(from=list % items, to=newlist % items)
        newlist % num_items = list % num_items
        list % num_items = 0
    end function


    pure subroutine check_bounds(self, idx)
        class(list_t), intent(in) :: self
        integer, intent(in) :: idx

        if (idx < 1 .or. idx > self % num_items) then
            error stop 'index out of range'
        end if
    end subroutine


    function item_at(self, idx)
        class(list_t), intent(inout), target :: self
        integer, intent(in) :: idx
        class(list_item_t), pointer :: item_at

        call check_bounds(self, idx)
        item_at => self % items(idx)
    end function


    pure subroutine ptr_at(self, idx, ptr)
        class(list_t), intent(inout), target :: self
        integer, intent(in) :: idx
        class(*), pointer, intent(inout) :: ptr

        call check_bounds(self, idx)
        ptr => self % items(idx) % value
    end subroutine


    pure subroutine getcopy_at(self, idx, dest)
        class(list_t), intent(in) :: self
        integer, intent(in) :: idx
        class(*), allocatable, intent(inout) :: dest

        call check_bounds(self, idx)
        allocate(dest, source=self % items(idx) % value)
        ! dest = self % items(idx) % value
    end subroutine


    pure subroutine adjust(self, desired_capacity)
        class(list_t), intent(inout) :: self
        integer, intent(in) :: desired_capacity
        integer :: current_capacity

        if (desired_capacity < 0) error stop 'desired_capacity < 0'

        if (.not. allocated(self % items)) then

            allocate(self % items(max(desired_capacity, min_capacity)))

        else
            current_capacity = size(self % items)

            if (desired_capacity > current_capacity) then
                call self % realloc_items(max(desired_capacity, 2 * current_capacity))
            else if (desired_capacity == 0) then
                deallocate(self % items)
            else if (desired_capacity < current_capacity / 2 &
                .and. current_capacity > min_capacity) then
                call self % realloc_items(max(current_capacity / 2, min_capacity))
            end if
        end if
    end subroutine


    pure subroutine realloc_items(self, new_capacity)
        class(list_t), intent(inout) :: self
        integer, intent(in) :: new_capacity
        type(list_item_t), allocatable :: new_items(:)
        integer :: i

        allocate(new_items(new_capacity))

        do i = 1, self % num_items
            call move_alloc(from=self % items(i) % value, &
                to=new_items(i) % value)
        end do

        call move_alloc(from=new_items, to=self % items)

    end subroutine


    pure function length(self)
        class(list_t), intent(in) :: self
        integer :: length

        length = self % num_items

    end function


    pure subroutine insert_empty(self, position, length)
        class(list_t), intent(inout) :: self
        integer, intent(in) :: position
        integer, intent(in) :: length
        integer :: i, num_items

        num_items = self % num_items

        if (position < 1) error stop 'position < 1'
        if (length < 0) error stop 'length < 0'
        if (position > num_items + 1) error stop 'position > num_items + 1'

        if (length == 0) return

        call self % adjust(num_items + length)

        do i = num_items, position, -1
            call move_alloc(from = self % items(i) % value, &
                to = self % items(i+length) % value)
        end do

        self % num_items = num_items + length

    end subroutine


    pure subroutine insert(self, item_value, position)
        class(list_t), intent(inout) :: self
        class(*), intent(in) :: item_value
        integer, intent(in), optional :: position
        integer :: position_

        position_ = self%num_items + 1
        if (present(position)) position_ = position

        call self % insert_empty(position_, 1)

        allocate(self % items(position_) % value, &
            source=item_value)

    end subroutine


    function insert_item(self, position) result(item_ptr)
        class(list_t), intent(inout) :: self
        integer, intent(in), optional :: position
        integer :: position_
        type(list_item_t), pointer :: item_ptr

        position_ = self%num_items + 1
        if (present(position)) position_ = position

        call self % insert_empty(position_, 1)
        item_ptr => self % item_at(position_)

    end function


    pure subroutine extend_array(self, item_values, position)
        class(list_t), intent(inout) :: self
        class(*), intent(in) :: item_values(:)
        integer, intent(in), optional :: position
        integer :: position_, length, i

        length = size(item_values)

        position_ = self%num_items + 1
        if (present(position)) position_ = position

        call self % insert_empty(position_, length)

        do i = 1, length
            allocate(self % items(position_ + (i - 1)) % value, &
                source=item_values(i))
        end do
    end subroutine


    pure subroutine extend_list(self, other, position)
        class(list_t), intent(inout) :: self
        class(sequence_t), intent(in) :: other
        integer, intent(in), optional :: position
        integer :: position_, length, i

        length = size(other)

        position_ = self%num_items + 1
        if (present(position)) position_ = position

        call self % insert_empty(position_, length)

        select type(other)
          class is (templist_t)
            do i = 1, length
                call move_alloc(from=other % items(i) % value, &
                    to=self % items(position_ + (i - 1)) % value)
            end do
          class default
            do i = 1, length
                call other % getcopy_at(i, self % items(position_ + (i - 1)) % value)
            end do
        end select
    end subroutine


    pure subroutine remove(self, position, length)
        class(list_t), intent(inout) :: self
        integer, intent(in) :: position
        integer, intent(in), optional :: length
        integer :: i, length_

        length_ = 1; if (present(length)) length_ = length

        if (position < 1) error stop 'position < 1'
        if (length_ < 1) error stop 'length < 1'
        if (position > self % num_items) &
            error stop 'position > num_items'
        if (position + length_ > self % num_items + 1) &
            error stop 'position + length > num_items + 1'

        self % num_items = self % num_items - length_

        do i = position, self % num_items
            if (allocated(self % items(i) % value)) &
                deallocate(self % items(i) % value)
            call move_alloc(from = self % items(i+length_) % value, &
                to = self % items(i) % value)
        end do

        call self % adjust(self % num_items)

    end subroutine


    pure subroutine erase(self)
        class(list_t), intent(inout) :: self

        if (allocated(self % items)) deallocate(self % items)
        self % num_items = 0

    end subroutine


    pure subroutine pack_list_common(sequence, indices, new_templist)
        class(list_t), intent(in) :: sequence
        type(templist_t), intent(out) :: new_templist
        integer, intent(in) :: indices(:)
        integer :: i, num_items

        num_items = size(indices)

        allocate(new_templist % items(num_items))

        select type (sequence)
          class is (templist_t)
            do i = 1, num_items
                call move_alloc(from=sequence % items(indices(i)) % value, &
                    to=new_templist % items(i) % value)
            end do
          class default
            do i = 1, num_items
                call sequence % getcopy_at(indices(i), dest=new_templist % items(i) % value)
            end do
        end select

        new_templist % num_items = num_items

    end subroutine


    pure function subset_list(sequence, indices) result(new_templist)
        class(list_t), intent(in) :: sequence
        integer, intent(in) :: indices(:)
        type(templist_t) :: new_templist

        call pack_list_common(sequence, indices, new_templist)

    end function


    pure function pack_list(sequence, mask) result(new_templist)
        class(list_t), intent(in) :: sequence
        logical, intent(in) :: mask(:)
        type(templist_t) :: new_templist
        integer :: i

        call pack_list_common(sequence, &
            pack([(i, i = 1, sequence % length())], mask), &
            new_templist)
    end function


    pure function filter_list(sequence, condition) result(new_templist)
        class(list_t), intent(in) :: sequence
        procedure(filter_proto) :: condition
        type(templist_t) :: new_templist
        logical, allocatable :: mask(:)
        integer :: i

        mask = [(condition(sequence%items(i)%value), &
            i = 1, sequence % length())]

        call pack_list_common(sequence, &
            pack([(i, i = 1, sequence % length())], mask), &
            new_templist)

    end function


    elemental function slice_list(sequence, from, to, stride) result(new_templist)
        class(list_t), intent(in) :: sequence
        integer, intent(in), optional :: from, to, stride
        integer :: from_, to_, stride_, i
        type(templist_t) :: new_templist

        stride_ = 1; if (present(stride)) stride_ = stride
        if (stride_ == 0) error stop

        from_ = merge(1, sequence % length(), stride_ > 0)
        if (present(from)) from_ = from

        to_ = merge(sequence % length(), 1, stride_ > 0)
        if (present(to)) to_ = to

        call pack_list_common(sequence, &
            [(i, i = from_, to_, stride_)], &
            new_templist)

    end function


    pure function concat_list_list(list1, list2) result(list3)
        class(list_t), intent(in) :: list1, list2
        type(templist_t) :: list3

        call list3 % extend(list1)
        call list3 % extend(list2)
    end function


    pure function concat_list_arr(list1, list2) result(list3)
        class(list_t), intent(in) :: list1
        class(*), intent(in) :: list2(:)
        type(templist_t) :: list3

        call list3 % extend(list1)
        call list3 % extend(list2)
    end function


    pure function concat_arr_list(list1, list2) result(list3)
        class(*), intent(in) :: list1(:)
        class(list_t), intent(in) :: list2
        type(templist_t) :: list3

        call list3 % extend(list1)
        call list3 % extend(list2)
    end function


    elemental subroutine assign_list(self, other)
        class(list_t), intent(inout) :: self
        class(list_t), intent(in) :: other

        call self % erase
        call self % extend(other)
    end subroutine

end module

