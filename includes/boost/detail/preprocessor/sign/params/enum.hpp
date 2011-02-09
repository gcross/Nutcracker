
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_HPP_

#include "size.hpp"
#include "../param.hpp"
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/logical/and.hpp>

#define BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_O_(z, n, op_data_params) \
    BOOST_PP_TUPLE_ELEM(3, 0, op_data_params)(z, \
            BOOST_DETAIL_PP_SIGN_PARAM(n, \
                    BOOST_PP_TUPLE_ELEM(3, 2, op_data_params)), \
            BOOST_PP_TUPLE_ELEM(3, 1, op_data_params) )

#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM_O_(z, n, op_data_params) \
    BOOST_PP_TUPLE_ELEM(3, 0, op_data_params)(z, \
            BOOST_DETAIL_PP_SIGN_DEDUCED_PARAM(n, \
                    BOOST_PP_TUPLE_ELEM(3, 2, op_data_params)), \
            BOOST_PP_TUPLE_ELEM(3, 1, op_data_params) )

// Induced params.

#define BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_Z(z, op_macro, data, params) \
    BOOST_PP_ENUM_ ## z(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_O_, \
            (op_macro, data, params) ) \

// Expand to `op_macro(z, param0, data), op_macro(z, param1, data), ...`.
#define BOOST_DETAIL_PP_SIGN_PARAMS_ENUM(op_macro, data, params) \
    BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_Z(1, op_macro, data, params)

// Deduced params.

#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM_Z( \
        z, op_macro, data, params) \
    BOOST_PP_ENUM_ ## z(BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM_O_, \
            (op_macro, data, params) )

#define BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM(op_macro, data, params) \
    BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM_Z(1, op_macro, data, params)

// All (induced and decuded) params.

#define BOOST_DETAIL_PP_SIGN_ALL_PARAMS_ENUM_Z(z, op_macro, data, params) \
    BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_Z(z, op_macro, data, params) \
    BOOST_PP_COMMA_IF(BOOST_PP_AND( \
            BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), \
            BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_SIZE(params))) \
    BOOST_DETAIL_PP_SIGN_DEDUCED_PARAMS_ENUM_Z(z, op_macro, data, params) 

#define BOOST_DETAIL_PP_SIGN_ALL_PARAMS_ENUM(op_macro, data, params) \
    BOOST_DETAIL_PP_SIGN_ALL_PARAMS_ENUM_Z(1, op_macro, data, params)

#endif // #include guard

