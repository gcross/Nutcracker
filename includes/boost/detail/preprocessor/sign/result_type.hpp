
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_RESULT_TYPE_HPP_
#define BOOST_DETAIL_PP_SIGN_RESULT_TYPE_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"
#include <boost/detail/preprocessor/keyword/is_void.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>

#define BOOST_DETAIL_PP_SIGN_RESULT_TYPE(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_RESULT_TYPE_INDEX, sign)

#define BOOST_DETAIL_PP_SIGN_IS_VOID_RESULT(sign) \
    BOOST_DETAIL_PP_KEYWORD_IS_VOID(BOOST_DETAIL_PP_SIGN_RESULT_TYPE( \
            sign))

// Constructors and destructors have no result.
#define BOOST_DETAIL_PP_SIGN_HAS_RESULT(sign) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY(BOOST_DETAIL_PP_SIGN_RESULT(sign)))

#endif // #include guard

