
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_START_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_START_HPP_

#include "../aux_/parser/sign_nil.hpp"
#include "../aux_/parser/err_msg.hpp"
#include "../aux_/parser/err_none.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/facilities/expand.hpp>

// If no error, specify error = (0, ...). Seq cannot be empty -- use (NIL).
#define BOOST_DETAIL_PP_SIGN_PARSE_START_(seq, err) \
    ( \
        seq /* cannot be empty seq (must have already added NIL) */ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_NIL \
    , \
        err \
    )

// If non empty seq (size > 0) has empty elem `()...`.
#define BOOST_DETAIL_PP_SIGN_PARSE_START_IS_SEQ_EMPTY_ELEM_(seq) \
    BOOST_PP_IIF(BOOST_PP_IS_EMPTY(BOOST_PP_SEQ_ELEM(0, seq)), \
        1 /* seq elem is empty `()...` */ \
    , \
        0 \
    )

// If seq is empty -- either emopty seq `` or empty elem `()...` (C99 only).
#define BOOST_DETAIL_PP_SIGN_PARSE_START_IS_SEQ_EMPTY_(seq) \
    BOOST_PP_IF(BOOST_PP_SEQ_SIZE(seq), \
        BOOST_DETAIL_PP_SIGN_PARSE_START_IS_SEQ_EMPTY_ELEM_ \
    , \
        1 /* seq itself is empty `` (size == 0) */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(seq)

#define BOOST_DETAIL_PP_SIGN_PARSE_START_HANDLE_EMPTY_(seq, empty_err) \
    BOOST_PP_EXPAND( \
    BOOST_DETAIL_PP_SIGN_PARSE_START_ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSE_START_IS_SEQ_EMPTY_(seq), \
        /* (NIL) to handle empty seq */ \
        ( (BOOST_PP_NIL), empty_err ) \
    , \
        /* (NIL) to be consistent with empty seq "last elem is NIL" */ \
        ( seq (BOOST_PP_NIL), BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_NONE ) \
    ))

// Allow seq to be empty.
#define BOOST_DETAIL_PP_SIGN_PARSE_START(seq) \
    BOOST_DETAIL_PP_SIGN_PARSE_START_HANDLE_EMPTY_(seq, \
            /* no error even if empty */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_NONE)

// If seq is empty, report specified `empty_error`.
#define BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY(seq, empty_error_msg) \
    BOOST_DETAIL_PP_SIGN_PARSE_START_HANDLE_EMPTY_(seq, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_ERR_MSG(empty_error_msg))

#endif // #include guard

