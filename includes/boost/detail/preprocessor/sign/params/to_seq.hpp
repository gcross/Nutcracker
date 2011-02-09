
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ_HPP_

#include "size.hpp"
#include "fold_left.hpp"
#include <boost/preprocessor/seq/seq.hpp> // For TAIL.
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ_( \
        op_macro, params, params_fold_left_macro) \
    BOOST_PP_SEQ_TAIL( /* remove `PP_NIL`, if got here size >= 1 */ \
            params_fold_left_macro(op_macro, \
                    (BOOST_PP_NIL), /* handle empty */ \
                    params))

// Public APIs.

// Expand to `seq` from `op_macro(s, op_macro(s, seq, param0), param1)...`.
// (Automatically handles empty seq and empty params -- expansion done only
// when params not empty, seq start as (PP_EMPTY) which is then removed.)
#define BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ(op_macro, params) \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
        BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ_ \
    , \
        BOOST_PP_TUPLE_EAT(3) \
    )(op_macro, params, BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT) \

// Same for deduced params.
#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_TO_SEQ(op_macro, params) \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params), \
        BOOST_DETAIL_PP_SIGN_PARAMS_TO_SEQ_ \
    , \
        BOOST_PP_TUPLE_EAT(3) \
    )(op_macro, params, BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_FOLD_LEFT) \

#endif // #include guard

