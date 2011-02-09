
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_HPP_

#include "deduced.hpp"
#include "all.hpp"
#include "tag_namespace.hpp"
#include "../param_.hpp"
#include "../param_/is_classified.hpp"
#include <boost/detail/preprocessor/keyword/parameter/is_deduced.hpp>
#include <boost/detail/preprocessor/keyword/is_namespace.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/logical/or.hpp>

// Named params.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_O_(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM(seq_sign_err, BOOST_PP_DEC(d), \
            0, /* named do not support auto/register storage classifiers */ \
            1, /* named */ \
            0 /* no template param */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_D(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_DEDUCED_D(d, \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_D(d, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_NAMED_O_, \
        BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_TAG_NAMESPACE( \
            seq_sign_err \
        ) \
    ))

// Is named token (no seq_sign_err).

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_IS_NAMED_TOKEN(token) \
    BOOST_PP_OR( \
          BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE(token) \
        , BOOST_PP_OR( \
              BOOST_DETAIL_PP_KEYWORD_IS_DEDUCED(token) \
            , BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM_IS_IO_TOKEN(token) \
        ) \
    )

#endif // #include guard

