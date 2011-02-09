
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_MEMBER_HPP_
#define BOOST_DETAIL_PP_SIGN_MEMBER_HPP_

#include "access.hpp"

#define BOOST_DETAIL_PP_SIGN_IS_MEMBER(sign) \
    /* IMPLEMENTATION: It is a member function iff it has non-empty access. */ \
    BOOST_DETAIL_PP_SIGN_HAS_ACCESS(sign)

#endif // #include guard

