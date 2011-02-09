
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_ACCESS_HPP_
#define BOOST_DETAIL_PP_SIGN_ACCESS_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/keyword/is_public.hpp>
#include <boost/detail/preprocessor/keyword/is_protected.hpp>
#include <boost/detail/preprocessor/keyword/is_private.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// Access.

#define BOOST_DETAIL_PP_SIGN_ACCESS(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ACCESS_INDEX, sign) \
            (/* expand empty */)

#define BOOST_DETAIL_PP_SIGN_HAS_ACCESS(sign) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY(BOOST_DETAIL_PP_SIGN_ACCESS(sign)))

// Public, protected, or private.

#define BOOST_DETAIL_PP_SIGN_ACCESS_IS_NOTEMPTY_(sign, is_macro) \
    is_macro(BOOST_DETAIL_PP_SIGN_ACCESS(sign))

#define BOOST_DETAIL_PP_SIGN_ACCESS_IS_(sign, is_macro) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_HAS_ACCESS(sign), \
        BOOST_DETAIL_PP_SIGN_ACCESS_IS_NOTEMPTY_ \
    , \
        0 /* false when no access (empty) */ \
        BOOST_PP_TUPLE_EAT(2) \
    )(sign, is_macro)

#define BOOST_DETAIL_PP_SIGN_IS_PUBLIC(sign) \
    BOOST_DETAIL_PP_SIGN_ACCESS_IS_(sign, \
            BOOST_DETAIL_PP_KEYWORD_IS_PUBLIC)

#define BOOST_DETAIL_PP_SIGN_IS_PROTECTED(sign) \
    BOOST_DETAIL_PP_SIGN_ACCESS_IS_(sign, \
            BOOST_DETAIL_PP_KEYWORD_IS_PROTECTED)

#define BOOST_DETAIL_PP_SIGN_IS_PRIVATE(sign) \
    BOOST_DETAIL_PP_SIGN_ACCESS_IS_(sign, \
            BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE)

#endif // #include guard

