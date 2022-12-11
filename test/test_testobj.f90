program test_testobj

    use testobject_m

    type :: obj_wrapper
        type(testobj_t) :: obj
    end type

    type :: obj_wrapper_assign
        type(testobj_w_assignment_t) :: obj
    end type

    block
        type(testobj_t) :: obj1, obj2

        write (*, '(/,a,/)') 'default assignment'

        obj1 = testobj_t('cat')
        obj2 = testobj_t('dog')

        print *, obj1 % ident(), ', ', obj2 % ident()

        obj2 = obj1 

        print *, obj1 % ident(), ', ', obj2 % ident()
    end block

    block
        type(testobj_w_assignment_t) :: obj1, obj2

        write (*, '(/,a,/)') 'overloaded assignment'

        obj1 = testobj_w_assignment_t('cat')
        obj2 = testobj_w_assignment_t('dog')

        print *, obj1 % ident(), ', ', obj2 % ident()

        obj2 = obj1 

        print *, obj1 % ident(), ', ', obj2 % ident()
    end block

    block
        type(obj_wrapper) :: obj1, obj2

        write (*, '(/,a,/)') 'default assignment -- derived type'

        obj1 % obj = testobj_t('cat')
        obj2 % obj = testobj_t('dog')

        print *, obj1 % obj % ident(), ', ', obj2 % obj % ident()

        obj2 = obj1 

        print *, obj1 % obj % ident(), ', ', obj2 % obj % ident()
    end block

    block
        type(obj_wrapper_assign) :: obj1, obj2

        write (*, '(/,a,/)') 'overloaded assignment -- derived type'

        obj1 % obj = testobj_w_assignment_t('cat')
        obj2 % obj = testobj_w_assignment_t('dog')

        print *, obj1 % obj % ident(), ', ', obj2 % obj % ident()

        obj2 = obj1 

        print *, obj1 % obj % ident(), ', ', obj2 % obj % ident()
    end block

    block
        type(testobj_w_assignment_t), allocatable :: obj, obj2

        write (*, '(/,a,/)') 'overloaded assignment -- allocate source'

        allocate(obj, source=testobj_w_assignment_t('hello'))
        print *, obj % ident()

        allocate(obj2)
        obj2 = testobj_w_assignment_t('kitty')
        print *, obj2 % ident()
        end block

    block
    end block

    block
    end block

end program
