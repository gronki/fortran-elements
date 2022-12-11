program test_dict

    use str_hash_m
    use str_dict_m
    implicit none

    type(default_hash_t) :: hash

    print *, hash % modulo
    print *, hash % hash('foo'), hash % hash('bar')
    print *, hash_dict_key(hash, [ &
        dict_key_t :: &
        dict_key_t(key='miala'), &
        dict_key_t(key='matka'), &
        dict_key_t(key='syna') &
        ])

end program
