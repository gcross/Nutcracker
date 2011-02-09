
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_FIRST_N_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_FIRST_N_HPP_

#include "../aux_/params_indexes.hpp"
#include <boost/preprocessor/seq/first_n.hpp>
#include <boost/preprocessor/array/elem.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAMS_FIRST_N(n, params) \
    /* don't use `PP_ARRAY_REPLACE` for reentrancy (and MSVC problems) */ \
    (BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDEX_MAX, ( \
        BOOST_PP_ARRAY_ELEM(BOOST_DETAIL_PP_SIGN_AUX_PARAMS_TAG_INDEX, \
                params) \
    , \
        BOOST_PP_SEQ_FIRST_N(BOOST_PP_INC(n), /* inc keeps leading NIL */ \
                BOOST_PP_ARRAY_ELEM( \
                        BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDUCED_INDEX, \
                        params)) \
    , \
        BOOST_PP_ARRAY_ELEM(BOOST_DETAIL_PP_SIGN_AUX_PARAMS_DEDUCED_INDEX, \
                params) \
    ))

#endif // #include guard

