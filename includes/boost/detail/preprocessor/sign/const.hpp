
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_CONST_HPP_
#define BOOST_DETAIL_PP_SIGN_CONST_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>

#define BOOST_DETAIL_PP_SIGN_CONST(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_CONST_INDEX, sign) \
            (/* expand empty */)

#define BOOST_DETAIL_PP_SIGN_IS_CONST(sign) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY(BOOST_DETAIL_PP_SIGN_CONST(sign)))

#endif // #include guard

