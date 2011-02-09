
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_HPP_

#include "return.hpp"
#include "if.hpp"
#include "../parser/seq_next.hpp"
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_HANDLE_EMPTY_( \
        seq_sign_err, append_empty) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_FWD(seq_sign_err, \
            /* return `toekn [EMPTY]` */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err) \
            BOOST_PP_EXPR_IF(append_empty, BOOST_PP_EMPTY) )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_HANDLE_EMPTY_( \
            seq_sign_err, 1 /* append EMPTY */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_NOEMPTY_( \
        seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_HANDLE_EMPTY_( \
            seq_sign_err, 0 /* do not append EMPTY */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_ERR_(seq_sign_err, op_error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_TUPLE_ELEM(2, 1, op_error))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OK_(seq_sign_err, op_error) \
    BOOST_PP_TUPLE_ELEM(2, 0, op_error)(seq_sign_err) /* op(sse) */

// Public API.
            
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OP( \
        seq_sign_err, check_macro, op_macro, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_IF_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_ERR_, /* if empty, err */ \
            check_macro, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OK_ /* if check, OK */ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_ERR_ /* if !check, err */ \
    , (op_macro, error) )

// Used when optional tokens are required (append EMPTY).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ( \
        seq_sign_err, check_macro, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OP(seq_sign_err, check_macro, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_, error)

// Used for tokens which are ALWAYS required (do not append EMPTY).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_NOEMPTY( \
        seq_sign_err, check_macro, error) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_OP(seq_sign_err, check_macro, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_REQ_DEFAULT_OP_NOEMPTY_, error)

#endif // #include guard

