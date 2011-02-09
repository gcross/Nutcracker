
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSER_APPLY_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSER_APLLY_HPP_

#include "sign.hpp"
#include "err.hpp"
#include <boost/preprocessor/control/iif.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSER_APPLY_OK_( \
        seq_sign_err, op_macro, err_macro, data) \
    op_macro(BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err), data)

#define BOOST_DETAIL_PP_SIGN_PARSER_APPLY_ERR_( \
        seq_sign_err, op_macro, err_macro, data) \
    err_macro(BOOST_DETAIL_PP_SIGN_PARSER_ERR_MESSAGE(seq_sign_err), data)

// Apply specified operation macro: If no error then expand to
// `op_macro(sign, data)`, else to `err_macro(message, data)` (where
// `message` is a valid concatenable pp single token like `ERROR_abc_xyz`).
// This can be used to process parsed signature via `op_macro()` while ensuring
// parsing macro (usually computationally expensive) is expanded only once:
//     #define PARSE_(seq) ... // Parse `seq` into `sign_seq_err`.
//     #define CODE_(sign, data) ... // Generate code from parsed signature.
//     #define ERR_(msg, data) ; BOOST_MPL_ASSERT_MSG(false, msg, ()) ;
//     #define CODE(seq) PP_SIGN_PARSE_APPLY(PARSE_(seq), CODE_, ERR_, ~)
#define BOOST_DETAIL_PP_SIGN_PARSER_APPLY( \
        seq_sign_err, op_macro, err_macro, data) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err), \
        BOOST_DETAIL_PP_SIGN_PARSER_APPLY_ERR_ /* expand to error */ \
    , /* else */ \
        BOOST_DETAIL_PP_SIGN_PARSER_APPLY_OK_ /* expand to code */ \
    )(seq_sign_err, op_macro, err_macro, data)

#endif // #include guard

