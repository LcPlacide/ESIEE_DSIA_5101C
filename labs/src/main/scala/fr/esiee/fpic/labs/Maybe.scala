package fr.esiee.fpic.labs:

    enum Maybe[+T]:
        case Just(v: T)
        case Nothing extends Maybe[Nothing]    
    
    def flatMap[T, Q](f: T => Maybe[Q])(m: Maybe[T]): Maybe[Q] = m match     
        case Maybe.Just(v) => f(v)
        case Maybe.Nothing => Maybe.Nothing

    def bind[T,Q](a: Maybe[T])(b: Maybe[Q]): Maybe[Q] = b
