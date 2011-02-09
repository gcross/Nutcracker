
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_HPP_

#include "is_bind.hpp"
#include <boost/detail/preprocessor/keyword/is_const.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// IMPORTANT: This is not a local macro, do not #undef it.
#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_STRIP_CONST_const \
    /* must expand to nothing so to remove leading `const` */

// Assumes there is a leading `const ...` and removes it.
#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_STRIP_CONST_( \
        tokens) \
    BOOST_PP_CAT( \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_STRIP_CONST_, \
            tokens)

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_(tokens) \
    BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND( \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_STRIP_CONST_( \
            tokens))

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND(tokens) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_CONST(tokens), \
        BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_ \
    , \
        0 /* not `const`, so it cannot be `const binded` */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(tokens)

#endif // #include guard

