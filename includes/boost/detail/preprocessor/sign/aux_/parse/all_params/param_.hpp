
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_HPP_

#include "param_/copyable.hpp"
#include "../return.hpp"
#include "../../../parser/seq.hpp"
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/comparison/less.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/cat.hpp>

// n is number of param being parsed 1, 2, ... (used for error reporting).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM(seq_sign_err, n, \
        storage01, named01, template01) \
    /* named: min size 4 = (io-qual) (type) (name) ... (PP_NIL) */ \
    /* pos:   min size 3 =           (type) (name) ... (PP_NIL) */ \
    BOOST_PP_IIF(BOOST_PP_LESS(BOOST_PP_SEQ_SIZE( \
            BOOST_DETAIL_PP_SIGN_PARSER_SEQ(seq_sign_err)), \
                BOOST_PP_ADD(3, named01) /* 3 (pos) or 4 (named) */), \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
                BOOST_PP_IIF(named01, \
                    BOOST_PP_CAT(ERROR_missing_io_classifier_or_type_or_name_for_named_parameter_, n) \
                , BOOST_PP_IIF(template01, \
                    BOOST_PP_CAT(ERROR_missing_type_identifier_or_name_for_template_parameter_, n) \
                , \
                    BOOST_PP_CAT(ERROR_missing_type_or_name_for_positional_parameter_, n) \
                )) \
        ) \
        BOOST_PP_TUPLE_EAT(5) \
    , \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_MAYBE_COPYABLE \
    )(seq_sign_err, n, storage01, named01, template01)

#endif // #include guard

