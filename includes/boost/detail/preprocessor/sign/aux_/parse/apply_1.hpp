
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_1_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_1_HPP_

#include "return.hpp"
#include "../../parser/seq.hpp"
#include "../../parser/err.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/comparison/greater.hpp>
#include <boost/preprocessor/seq/size.hpp>

// IMPORTANT: Code repeated in "apply_1.hpp", ... to avoid reentrance issues.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_DEFAULT_ERR_1_( \
        seq_sign_err, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_missing_one_or_more_tokens_from_sequence) /* default error */

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_HANDLE_EMPTY_1_( \
        seq_sign_err, op_macro, empty_macro, data) \
    BOOST_PP_IIF(BOOST_PP_GREATER(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), 1), \
        op_macro \
    , \
        empty_macro /* empty if size <= 1 (because (PP_NIL) always there) */ \
    )(seq_sign_err, data)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_FWD_ERR_1_( \
        seq_sign_err, op_macro, empty_macro, data) \
    seq_sign_err /* foward existing err */

// Public API.

// If err, forward err; If empty, `empty_macro(...)`; Else, `op_macro(...)`.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1( \
        seq_sign_err, op_macro, empty_macro, data) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_FWD_ERR_1_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_HANDLE_EMPTY_1_ \
    )(seq_sign_err, op_macro, empty_macro, data)

// If err, forward err. If empty, default err. Else, `op_macro(sse, data)`.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_1( \
        seq_sign_err, op_macro, data) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY_1(seq_sign_err, op_macro, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_DEFAULT_ERR_1_, data)

#endif // #include guard

