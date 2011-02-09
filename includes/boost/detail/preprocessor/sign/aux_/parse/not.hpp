
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_HPP_

#include "return.hpp"
#include "if.hpp"
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_NO_(seq_sign_err, empty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(2, 0, empty_error) /* tokens for empty */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_YES_(seq_sign_err, empty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(2, 1, empty_error) /* errror */ )

// Public API.
    
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_WITH_EMPTY( \
        seq_sign_err, check_macro, empty_tokens, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_NO_, \
            check_macro, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_YES_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_NO_ \
    , (empty_tokens, error) )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT( \
        seq_sign_err, check_macro, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_NOT_WITH_EMPTY( \
            seq_sign_err, check_macro, \
            BOOST_PP_EMPTY /* use by default for empty */, error)

#endif // #include guard

