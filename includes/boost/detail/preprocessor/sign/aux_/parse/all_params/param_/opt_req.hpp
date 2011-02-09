
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPTREQ_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_OPTREQ_HPP_

#include "single.hpp"
#include "../../../parse/return.hpp"
#include "../../../../parser/seq.hpp"
#include <boost/detail/preprocessor/keyword/is_default.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/cat.hpp>

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_OPTIONAL_(seq_sign_err, n, \
        storage01, named01, template01, copyable01) \
    /* named: size 6= (io-qual) (copyable)(type) (name) (def)(val) (PP_NIL) */ \
    /* pos:   size 5= [storage] (copyable)(type) (name) (def)(val) (PP_NIL) */ \
    BOOST_PP_IIF(BOOST_PP_LESS( \
            BOOST_PP_SEQ_SIZE(BOOST_DETAIL_PP_SIGN_PARSER_SEQ( \
                    seq_sign_err)), \
            /* err: size < 5 (pos) / 6 (pos-copy or named) / 7 (named-copy) */ \
            BOOST_PP_ADD(BOOST_PP_ADD(5, BOOST_PP_OR(named01, storage01)), \
                    copyable01)), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err,  \
                BOOST_PP_IIF(named01, \
                    BOOST_PP_CAT(ERROR_missing_default_value_for_named_parameter_, n) \
                , BOOST_PP_IIF(template01, \
                    BOOST_PP_CAT(ERROR_missing_default_value_for_template_parameter_, n) \
                , /* give same error regardless of storage classifier */ \
                    BOOST_PP_CAT(ERROR_missing_default_value_for_positional_parameter_, n) \
                )) \
        ) \
        BOOST_PP_TUPLE_EAT(7) \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE \
    )(seq_sign_err, n, \
            storage01, named01, template01, copyable01, 1 /* has default */)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_REQUIRED_(seq_sign_err, n, \
        storage01, named01, template01, copyable01) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_SINGLE(seq_sign_err, n, \
            storage01, named01, template01, copyable01, 0 /* no default */)

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_OPT_OR_REQ(seq_sign_err, n, \
        storage01, named01, template01, copyable01) \
    /* named: size > 3/4, check is 3rd/4th elem is (default) */ \
    /* pos:   size > 2/3, check is 2nd/3rd elem is (default) */ \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_DEFAULT(BOOST_PP_SEQ_ELEM( \
            /* index is 3 (pos) / 4 (pos-copy or named) / 5 (named-copy) */ \
            BOOST_PP_ADD(BOOST_PP_ADD(2, BOOST_PP_OR(named01, storage01)), \
            copyable01), BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err))), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_OPTIONAL_ \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_REQUIRED_ \
    )(seq_sign_err, n, storage01, named01, template01, copyable01)

#endif // #include guard

