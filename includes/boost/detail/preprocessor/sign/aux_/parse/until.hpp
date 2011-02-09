
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_HPP_

#include "apply.hpp"
#include "../parser/sign_set.hpp"
#include "../parser/sign_replace_back.hpp"
#include "../parser/sign_back.hpp"
#include "../parser/seq_next.hpp"
#include "../parser/seq_skip.hpp"
#include "../parser/sign_append.hpp"
#include "../../parser/sign.hpp"
#include <boost/preprocessor/control/while.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/cat.hpp>

// Concatenate last elem of seq onto last elem of sign.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_CONCAT_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_SET(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSER_SIGN( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_REPLACE_BACK(seq_sign_err, \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_BACK(seq_sign_err) \
                    /* seq append next elem `(next)` */ \
                    ( BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT( \
                            seq_sign_err) ) \
            )))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_DEFAULT_OP_(n, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_SKIP( /* move to next in seq */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_CONCAT_(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_D_(seq_sign_err, d_op_pred) \
    /* use PP_CAT() (not ##) to expand tuple elem */ \
    BOOST_PP_CAT(BOOST_PP_WHILE_, BOOST_PP_TUPLE_ELEM(3, 0, d_op_pred))( \
            BOOST_PP_TUPLE_ELEM(3, 1, d_op_pred), /* pred_macro */ \
            BOOST_PP_TUPLE_ELEM(3, 2, d_op_pred), /* op_macro */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SIGN_APPEND(seq_sign_err, \
                    (BOOST_PP_NIL) /* handle empty seq */ ))
    
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_EMPTY_D_( \
        seq_sign_err, d_op_pred) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            (BOOST_PP_NIL) /* handle empty seq */ )

// Public API.

// Reentrant version.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_WITH_OP_D( \
        d, seq_sign_err, pred_macro, op_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_D_, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_EMPTY_D_, \
            (d, pred_macro, op_macro) )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_WITH_OP( \
        seq_sign_err, pred_macro, op_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_WITH_OP_D( \
            1, seq_sign_err, pred_macro, op_macro)

// Until pred_macro(n, sse), extract sequence from seq.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL(seq_sign_err, pred_macro) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_WITH_OP_D( \
            1, seq_sign_err, pred_macro, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_UNTIL_DEFAULT_OP_)

#endif // #include guard

