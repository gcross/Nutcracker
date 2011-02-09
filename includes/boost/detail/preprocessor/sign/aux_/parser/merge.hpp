
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE_HPP_

#include "../../parser/seq.hpp"
#include "../../parser/sign.hpp"
#include "../../parser/err.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/elem.hpp>

/** @todo Check if all of these merges are still used and remove eventually unused ones. */

// Merge two SSE as: seq as specified, sign of 1 followed by sign of 2, err from
// either 1 or 2 with non empty err.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE( \
        seq_sign_err1, seq_sign_err2, seq) \
    ( /* remaining seq */ \
        seq \
    , /* sign 1 followed by 2 */ \
        BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err1) /* seq append */ \
        /* `TAIL()` removes leading `(NIL)` (to handle empty seq) */ \
        BOOST_PP_SEQ_TAIL(BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err2)) \
    , /* err from either one that is not OK (or NONE if both OK) */ \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR( \
            BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR( \
                    seq_sign_err1), \
                seq_sign_err1 /* take err1 (because not OK) */ \
            , \
                seq_sign_err2 /* take err2 (might or not be OK) */ \
            ) \
        ) \
    )

// Merges only the 2nd element from sign2.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE_2ND_( \
        seq_sign_err1, seq_sign_err2, seq) \
    ( /* remaining seq */ \
        seq \
    , /* sign 1 followed by 2nd elem of sign 2 */ \
        BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err1) /* seq append */ \
        BOOST_PP_SEQ_ELEM(2, \
                /* take 2nd elem from sign2 (0 is NIL, 1 is 1st, 2 is 2nd) */ \
                BOOST_DETAIL_PP_SIGN_PARSER_SIGN(seq_sign_err2)) \
    , /* err from either one that is not OK (or NONE if both OK) */ \
        BOOST_DETAIL_PP_SIGN_PARSER_ERR( \
            BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR( \
                    seq_sign_err1), \
                seq_sign_err1 /* take err1 (because not OK) */ \
            , \
                seq_sign_err2 /* take err2 (might or not be OK) */ \
            ) \
        ) \
    )

// Merge two SSE as: seq from 2, sign of 1 followed by sign of 2, err from
// either 1 or 2 with non empty err.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE2( \
        seq_sign_err1, seq_sign_err2) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE( \
            seq_sign_err1, seq_sign_err2, \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err2))

// As merge 2 must skips 1st elem in sign2.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE2_2ND( \
        seq_sign_err1, seq_sign_err2) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_MERGE_2ND_( \
            seq_sign_err1, seq_sign_err2, \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err2))

#endif // #include guard

