
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_HPP_
#define BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_HPP_

#include "return.hpp"
#include "../parser/err.hpp"
#include "../parser/seq.hpp"
#include "../aux_/parsed/count.hpp"
#include "../aux_/parsed/indexes.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/comparison/not_equal.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// First applies common validation checks, if they pass apply additional
// validation checks as defined by the user in `validation_macro(seq_sign_err)`.
#define BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC( \
        seq_sign_err, sign_count, validation_macro) \
    /* common validation checks */ \
    /* forward existing error (if any) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARSER_HAS_ERR(seq_sign_err), \
        seq_sign_err \
        BOOST_PP_TUPLE_EAT(1) \
    /* when done, only seq elem left should be NIL (size != 1 invalid) */ \
    , BOOST_PP_IIF(BOOST_PP_NOT_EQUAL(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_PARSED_SEQ(seq_sign_err)), 1), \
        BOOST_DETAIL_PP_SIGN_VALIDATE_RETURN_ERR(seq_sign_err, \
                ERROR_unable_to_parse_all_tokens_because_of_invalid_syntax_or_order) \
        BOOST_PP_TUPLE_EAT(1) \
    /* when done, exactly ID_MAX elems in sign */ \
    , BOOST_PP_IIF(BOOST_PP_NOT_EQUAL( \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_COUNT(seq_sign_err), \
            sign_count), \
        BOOST_DETAIL_PP_SIGN_VALIDATE_RETURN_ERR(seq_sign_err, \
                ERROR_missing_one_or_more_signature_tokens) \
        BOOST_PP_TUPLE_EAT(1) \
    , /* user defined validation checks */ \
        validation_macro \
    )))(seq_sign_err)

#define BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_DECL( \
        seq_sign_err, validation_macro) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_INDEX_MAX, \
            validation_macro)

#define BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC_CLASS_DECL( \
        seq_sign_err, validation_macro) \
    BOOST_DETAIL_PP_SIGN_VALIDATE_BASIC(seq_sign_err, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSED_CLASS_INDEX_MAX, \
            validation_macro)

#endif // #include guard

