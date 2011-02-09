
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_POSITIONAL_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_POSITIONAL_HPP_

#include "deduced.hpp"
#include "tag_namespace.hpp"
#include "../param_.hpp"
#include "../param_/is_classified.hpp"
#include <boost/preprocessor/arithmetic/dec.hpp>

// Positional params (it is a positional param if it is not a named one).

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_POSITIONAL_O_( \
        d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM(seq_sign_err, BOOST_PP_DEC(d), \
            /* if storage classified by auto/register */ \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_STORAGE_TOKEN( \
                    BOOST_DETAIL_PP_SIGN_AUX_PARSER_SEQ_NEXT(seq_sign_err)),\
            0, /* no named param */ \
            0 /* no template param */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_POSITIONAL_D(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_NO( \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_D(d, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_POSITIONAL_O_, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE_NOT( \
            seq_sign_err \
        ) \
    ))

#endif // #include guard

