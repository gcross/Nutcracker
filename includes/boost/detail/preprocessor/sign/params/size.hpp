
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_SIZE_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_SIZE_HPP_

#include "../aux_/params_indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/array/elem.hpp>

// Induced params.
#define BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params) \
    BOOST_DETAIL_PP_NILSEQ_SIZE(BOOST_PP_ARRAY_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDUCED_INDEX, params))

// Deduced params.
#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params) \
    BOOST_DETAIL_PP_NILSEQ_SIZE(BOOST_PP_ARRAY_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_DEDUCED_INDEX, params))

// All params.
#define BOOST_DETAIL_PP_SIGN_ALL_PARAMS_SIZE(params) \
    BOOST_PP_ADD(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params))

#endif // #include guard

