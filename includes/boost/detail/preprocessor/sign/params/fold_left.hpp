
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_HPP_

#include "size.hpp"
#include "../param.hpp"
#include "../aux_/params_indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/seq/fold_left.hpp>
#include <boost/preprocessor/array/elem.hpp>
#include <boost/preprocessor/control/if.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_S_( \
        s, op_macro, state, params, params_size, params_index) \
    BOOST_PP_SEQ_FOLD_LEFT_ ## s(op_macro, state, \
            BOOST_DETAIL_PP_NILSEQ_TO_SEQ(BOOST_PP_ARRAY_ELEM( \
            params_index, params)))

#define BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_IDENTITY_S_( \
        s, op_macro, state, params, params_size, params_index) \
    state /* just forward original state */

// Induced params.

#define BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_S(s, op_macro, state, params) \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
        BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_S_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_IDENTITY_S_ \
    )(s, op_macro, state, params, \
            BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDUCED_INDEX)

// Expand to `op_macro(s, op_macro(s, state, param0), param1)...`.
#define BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT(op_macro, state, params) \
    BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_S(1, op_macro, state, params)

// Deduced params.

#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_FOLD_LEFT_S( \
        s, op_macro, state, params) \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params), \
        BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_S_ \
    , \
        BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT_IDENTITY_S_ \
    )(s, op_macro, state, params, \
            BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_DEDUCED_INDEX)

// Expand to `op_macro(s, op_macro(s, state, param0), param1)...`.
#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_FOLD_LEFT( \
        op_macro, state, params) \
    BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_FOLD_LEFT_S( \
            1, op_macro, state, params)

#endif // #include guard

