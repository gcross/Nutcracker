
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_HPP_

#include "../parser/err_set.hpp"
#include "../parser/sign_append.hpp"
#include "../parser/seq_set.hpp"
#include "../parser/seq_skip.hpp"
#include "../parser/seq_next.hpp"

// Return error (no change to seq or sign).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR( \
        seq_sign_err, error_message) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_SET(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_MSG(error_message))

// Return OK, append result to sign, do not move seq.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, result) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_APPEND(seq_sign_err, result)

// Return OK, append result to sign, set seq to specified reaming-seq.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_OK( \
        seq_sign_err, result, remaining_seq) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SET( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL( \
            seq_sign_err, result), remaining_seq)

// Return OK, append result to sign, move seq forward of 1 token.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, result) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL( \
            seq_sign_err, result))

// Return OK, append next in seq to sign, move seq forward of 1 token.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_NEXT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err))

#endif // #include guard

