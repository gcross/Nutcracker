
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARSE_BODY_HPP_
#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_HPP_

#include "../parser/seq.hpp"
#include "../aux_/parse/return.hpp"
#include "../aux_/parse/apply.hpp"
#include <boost/detail/preprocessor/seq.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/remove.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/arithmetic/sub.hpp>

#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_OK_(seq_sign_err, body_index) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_OK(seq_sign_err, \
            BOOST_PP_SEQ_ELEM(body_index, \
                    BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), \
            BOOST_PP_SEQ_REMOVE(BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                    seq_sign_err), body_index))

#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_(seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_PARSE_BODY_OK_(seq_sign_err, \
            /* got here if !empty => size > 1 (>=2) as NIL always there, */ \
            /* then it is safe to access the body as the 2nd last element */ \
            /* (i.e., just before added trailing NIL) */ \
            BOOST_PP_SUB(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), 2))

#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_EMPTY_(seq_sign_err, unused) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_missing_body)

// Differently from all other parses, the body is NOT parsed "in place". It
// is instead always assumed to be at the back of seq (so the BACK postfix).
// Thus, this parse operation can be safety used as the first one.
#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_REQ(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_PARSE_BODY_, \
            BOOST_DETAIL_PP_SIGN_PARSE_BODY_EMPTY_, \
            ~)

#define BOOST_DETAIL_PP_SIGN_PARSE_BODY_BACK_NOT(seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_STILL(seq_sign_err, \
            BOOST_PP_NIL /* don't use EMPTY (for MSVC expansion issues) */ )

#endif // #include guard

