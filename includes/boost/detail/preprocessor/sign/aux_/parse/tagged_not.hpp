
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_HPP_

#include "return.hpp"
#include "apply_1.hpp" // To avoid reentrancy issues.
#include "if_1.hpp" // To avoid reentrancy issues.
#include <boost/preprocessor/tuple/elem.hpp>

// Parse no tagged token (not present).

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_ERR_( \
        seq_sign_err, istag_empty_error) \
    BOOST_DETAIL_PP_SIGN_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(3, 2, istag_empty_error) /* error */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_OK_( \
        seq_sign_err, istag_empty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(3, 1, istag_empty_error) /* empty_tokens */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_( \
        seq_sign_err, istag_empty_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_1(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(3, 0, istag_empty_error), /* istag_macro */ \
        /* if is tag then error (no-tagged is NOT supposed to match) */ \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_ERR_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_OK_ \
    , istag_empty_error)

// Public API.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT( \
        seq_sign_err, istag_macro, empty_tokens, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1(seq_sign_err, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_, \
        /* handle empty seq as OK -- if empty, there is not tagged token */ \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_TAGGED_NOT_OK_, \
        (istag_macro, empty_tokens, error) )

#endif // #include guard

