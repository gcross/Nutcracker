
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_POSTCONDITION_HPP_
#define BOOST_DETAIL_PP_SIGN_POSTCONDITION_HPP_

#include "aux_/parsed/elem.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/detail/preprocessor/keyword/is_void.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// Postconditions' result.

#define BOOST_DETAIL_PP_SIGN_POSTCONDITION_RESULT(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_POSTCONDITION_RESULT_ID, sign) \
            (/* expand empty */)

// Expand to 1 iff has a result-name (but it could be `void`). result-name == 
// `void` must be allowed to handle `typedef void myvoid;` and similar.
#define BOOST_DETAIL_PP_SIGN_POSTCONDITION_HAS_RESULT(sign) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY( \
            BOOST_DETAIL_PP_SIGN_POSTCONDITION_RESULT(sign)))

#define BOOST_DETAIL_PP_SIGN_POSTCONDITION_IS_RESULT_NONVOID_(sign) \
    /* if got here, result not empty */ \
    BOOST_PP_NOT(BOOST_DETAIL_PP_KEYWORD_IS_VOID( \
            BOOST_DETAIL_PP_SIGN_POSTCONDITION_RESULT(sign)))

#define BOOST_DETAIL_PP_SIGN_POSTCONDITION_HAS_NONVOID_RESULT(sign) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_POSTCONDITION_HAS_RESULT(sign), \
        /* has result-name then check if result-name == `void` or not */ \
        BOOST_DETAIL_PP_SIGN_POSTCONDITION_IS_RESULT_NONVOID_ \
    , \
        0 /* has no result-name so doesn't have result-name != `void` */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(sign)

// Postconditions.

#define BOOST_DETAIL_PP_SIGN_POSTCONDITION(sign) \
    BOOST_DETAIL_PP_NILSEQ_TO_SEQ(BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_POSTCONDITION_INDEX, sign))

#define BOOST_DETAIL_PP_SIGN_HAS_POSTCONDITION(sign) \
    BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_POSTCONDITION_ID, sign)))

#endif // #include guard

