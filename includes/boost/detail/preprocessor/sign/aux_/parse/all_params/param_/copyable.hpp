
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_COPYABLE_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_COPYABLE_HPP_

#include "opt_req.hpp"
#include "../../../parse/return.hpp"
#include "../../../../parser/seq.hpp"
#include <boost/detail/preprocessor/keyword/contract/is_copyable.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/cat.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_COPYABLE_(seq_sign_err, n, \
        storage01, named01, template01) \
    /* if got here, template01 is 0 because tparam cannot be (copyable) */ \
    /* named: min size 5 = (io-qual) (copyable) (type) (name) ... (PP_NIL) */ \
    /* pos:   min size 4 =           (copyable) (type) (name) ... (PP_NIL) */ \
    BOOST_PP_IIF(BOOST_PP_LESS( \
            BOOST_PP_SEQ_SIZE(BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                    seq_sign_err)), \
            /* add expands to 4 (pos) or 5 (named) */ \
            BOOST_PP_ADD(4, BOOST_PP_OR(storage01, named01))), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
                BOOST_PP_IIF(named01, \
                    BOOST_PP_CAT(ERROR_missing_io_classifier_or_type_or_name_for_copyable_named_parameter_, n) \
                , BOOST_PP_IIF(storage01, \
                    BOOST_PP_CAT(ERROR_missing_storage_classifier_or_type_or_name_for_copyable_positional_parameter_, n) \
                , \
                    BOOST_PP_CAT(ERROR_missing_type_or_name_for_copyable_positional_parameter_, n) \
                )) \
        ) \
        BOOST_PP_TUPLE_EAT(6) \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_OPT_OR_REQ \
    )(seq_sign_err, n, storage01, named01, template01, 1 /* copyable */)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_NONCOPYABLE_(seq_sign_err, n, \
        storage01, named01, template01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_OPT_OR_REQ(seq_sign_err, n, \
            storage01, named01, template01, 0 /* non-copyable */)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_MAYBE_COPYABLE( \
        seq_sign_err, n, storage01, named01, template01) \
    /* named: size > 3/2, check if 2nd elem is (copyable) */ \
    /* pos:   size > 2/1, check if 1st elem is (copyable) */ \
    BOOST_PP_IIF( \
            BOOST_PP_AND( \
                  BOOST_PP_NOT(template01) \
                /* tparam always parsed as non-copyable */ \
                , BOOST_DETAIL_PP_KEYWORD_IS_COPYABLE(BOOST_PP_SEQ_ELEM( \
                        /* add expands to 0/1 (pos) or 1/2 (named) */ \
                        BOOST_PP_ADD(0, BOOST_PP_OR(storage01, named01)), \
                        BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err))) \
            ), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_COPYABLE_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_NONCOPYABLE_ \
    )(seq_sign_err, n, storage01, named01, template01)

#endif // #include guard

