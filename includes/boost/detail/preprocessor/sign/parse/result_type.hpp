
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_HPP_

#include "../aux_/parse/req.hpp"
#include "../aux_/parse/return.hpp"

#define BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_ANY_TOKEN_(token) \
    1 /* always true for any token can be result-type */

#define BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_REQ(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_NOEMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_ANY_TOKEN_, \
            ERROR_missing_result_type)

#define BOOST_DETAIL_PP_SIGN_PARSE_RESULT_TYPE_NOT(seq_sign_err) \
    /* cannot use PARSER_NOT because any token can be result */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            void) /* no result must be `void` result */

#endif // #include guard

