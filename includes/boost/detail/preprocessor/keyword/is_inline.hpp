
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_IS_INLINE_HPP_
#define BOOST_DETAIL_PP_KEYWORD_IS_INLINE_HPP_

#include "check.hpp"

// These are not loacl macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_INLINE_inline (1)

#define BOOST_DETAIL_PP_KEYWORD_IS_INLINE(tokens) \
    BOOST_DETAIL_PP_KEYWORD_CHECK_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_INLINE_)

#endif // #include guard

