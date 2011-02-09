
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_FOR_EACH_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_FOR_EACH_HPP_

#include "size.hpp"
#include "../param.hpp"
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAMS_FOR_EACH_O_(z, n, op_params) \
    BOOST_PP_TUPLE_ELEM(2, 0, op_params) \
    (z, n, BOOST_DETAIL_PP_SIGN_PARAM(n, \
            BOOST_PP_TUPLE_ELEM(2, 1, op_params)))

// Expand in `op_macro(z, 0, param0) op_macro(z, 1, param1) ...`.
#define BOOST_DETAIL_PP_SIGN_PARAMS_FOR_EACH(op_macro, params) \
    BOOST_PP_REPEAT(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_PARAMS_FOR_EACH_O_, \
            (op_macro, params))

#endif // #include guard

