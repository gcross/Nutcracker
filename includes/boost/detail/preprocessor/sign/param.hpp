
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAM_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAM_HPP_

#include "aux_/params_indexes.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/array/elem.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAM_(index, n, params) \
    BOOST_DETAIL_PP_NILSEQ_ELEM(n, BOOST_PP_ARRAY_ELEM(index, params))

#define BOOST_DETAIL_PP_SIGN_PARAM(n, params) \
    BOOST_DETAIL_PP_SIGN_PARAM_( \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDUCED_INDEX, n, params)

#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAM(n, params) \
    BOOST_DETAIL_PP_SIGN_PARAM_( \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_DEDUCED_INDEX, n, params)

#endif // #include guard

