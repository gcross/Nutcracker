
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_HPP_

#include "is_classified.hpp"
#include "../../../parse/return.hpp"
#include "../../../../parser/seq.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/expr_if.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/rest_n.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/cat.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_DEFAULT_( \
        seq_sign_err, n, classified01, template01, copyable01) \
    BOOST_PP_SEQ_ELEM( \
            BOOST_PP_ADD(BOOST_PP_ADD(3, classified01), copyable01), \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_(seq_sign_err, n, \
        classified01, template01, copyable01, default01) \
    /* named: size >= 6/7 or 4/5 (last might be PP_NIL) */ \
    /* post:  size >= 5/6 or 3/4 (last might be PP_NIL) +1 if classified */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_OK(seq_sign_err, \
            ( /* io or auto/register classifier (optional for pos-params) */ \
                BOOST_PP_IIF(classified01, \
                    BOOST_PP_SEQ_ELEM \
                , \
                    BOOST_PP_TUPLE_EAT(2) \
                )(0, BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err))\
                BOOST_PP_EMPTY /* (for optional) */ \
            , /* copyable (optional) */ \
                BOOST_PP_EXPR_IF(copyable01, copyable) \
                BOOST_PP_EMPTY /* (for optional) */ \
            , /* type */ \
                BOOST_PP_SEQ_ELEM(BOOST_PP_ADD(BOOST_PP_ADD( \
                        0, classified01), copyable01), \
                        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)) \
            , /* name */ \
                BOOST_PP_SEQ_ELEM(BOOST_PP_ADD(BOOST_PP_ADD( \
                        1, classified01), copyable01), \
                        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)) \
            , /* default value (optional) */ \
                BOOST_PP_IIF(default01, \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_DEFAULT_ \
                , \
                    BOOST_PP_TUPLE_EAT(5) \
                )(seq_sign_err, n, classified01, template01, copyable01) \
                BOOST_PP_EMPTY /* (for optional) */ \
            ), \
            BOOST_PP_SEQ_REST_N( \
                    BOOST_PP_ADD(BOOST_PP_ADD(BOOST_PP_ADD( \
                            2, classified01), copyable01), \
                            BOOST_PP_IIF(default01, \
                                2 /* skip both (default)(val) */ \
                            , \
                                0 /* no default, skip nothing */ \
                            ) \
                    ), \
                    BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err) \
            ) \
    )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_OK_(seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    /* classified either by storage (auto/register) or i/o (in/out/inout) */ \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_(seq_sign_err, n, \
            BOOST_PP_OR(storage01, named01), template01, copyable01, default01)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_STORAGE_UNEXPECTED_ERR_( \
        seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_CAT(ERROR_unexpected_strorage_classifier_for_parameter_, n))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_STORAGE_EXPECTED_ERR_( \
        seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_CAT(ERROR_missing_storage_classifier_for_parameter_, n))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_IO_UNEXPECTED_ERR_( \
        seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_CAT(ERROR_specified_io_classifier_for_non_named_parameter_, n))
        
#define CONTRACT__PP_SIGN_AUX_PARSE_PARAM_SINGLE_IO_EXPECTED_ERR_( \
        seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            BOOST_PP_CAT(ERROR_missing_io_classifier_for_named_parameter_, n))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE(seq_sign_err, n, \
        storage01, named01, template01, copyable01, default01) \
    BOOST_PP_IIF( \
            BOOST_PP_AND( \
                  BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_IO_TOKEN( \
                        BOOST_PP_SEQ_ELEM( \
                                BOOST_PP_ADD(BOOST_PP_ADD(0, named01), \
                                        copyable01), \
                                BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                                        seq_sign_err))) \
                , BOOST_PP_NOT(named01) \
            ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_IO_UNEXPECTED_ERR_ \
    , BOOST_PP_IIF( \
            BOOST_PP_AND( \
                  named01 \
                , BOOST_PP_NOT( \
                        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_IO_TOKEN( \
                                BOOST_PP_SEQ_ELEM(0, \
                                        BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                                                seq_sign_err)))) \
            ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_IO_EXPECTED_ERR_ \
    , BOOST_PP_IIF( \
            BOOST_PP_AND( \
                  BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_STORAGE_TOKEN( \
                        BOOST_PP_SEQ_ELEM( \
                                BOOST_PP_ADD(BOOST_PP_ADD(0, storage01), \
                                        copyable01), \
                                BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                                        seq_sign_err))) \
                , BOOST_PP_NOT(storage01) \
            ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_STORAGE_UNEXPECTED_ERR_\
    , BOOST_PP_IIF( \
            BOOST_PP_AND( \
                  storage01 \
                , BOOST_PP_NOT( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_STORAGE_TOKEN(\
                            BOOST_PP_SEQ_ELEM(0, \
                                    BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                                            seq_sign_err)))) \
            ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_STORAGE_EXPECTED_ERR_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE_OK_ \
    ))))(seq_sign_err, n, \
            storage01, named01, template01, copyable01, default01)

#endif // #include guard

