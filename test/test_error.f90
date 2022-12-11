program test_error

    use error_m
    use error_type_m
    implicit none

    type(error_handler_t) :: err

    call baz(.false., err)
    call baz(.false.)
    call baz(.true., err)
    call baz(.true.)

contains

    subroutine foo(blowup, err)
        logical :: blowup
        type(error_handler_t), optional :: err

        print *, 'foo'

        if (blowup) call raise("error in foo", err)
    end subroutine

    subroutine bar(blowup, err)
        logical :: blowup
        type(error_handler_t), optional :: err

        print *, 'bar'

        call foo(blowup, err)
        if (caught(err)) return

    end subroutine

    subroutine baz(blowup, err)
        logical :: blowup
        type(error_handler_t), optional :: err

        print *, 'baz'

        call bar(blowup, err)

        SELECT TYPE(status => pop_status(err))
          CLASS IS (ERROR_T)
            print *, 'caught an error: ', status % to_str()
            return
          CLASS DEFAULT
            ! print *, status % to_str()
        END SELECT

    end subroutine

end program
