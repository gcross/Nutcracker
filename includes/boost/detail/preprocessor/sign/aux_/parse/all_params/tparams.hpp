
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_HPP_

#include "param_.hpp"
#include "params_/deduced.hpp"
#include "params_/tag_namespace.hpp"
#include "../apply.hpp"
#include "../return.hpp"
#include "../../../parse/start.hpp"
#include <boost/preprocessor/arithmetic/dec.hpp>

// This code re-uses the function parameter parse macros.

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_O_(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAM(seq_sign_err, BOOST_PP_DEC(d), \
            0, /* tparam does not have auto/register storage classifiers */ \
            0, /* tparam is not named (positional) */ \
            1 /* template parameter */ )

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_(seq_sign_err, d) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_PARAMS_ALL_D(d, \
            BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_O_, \
            /* Template params have no tag-namespace to parse by calling */ \
            /* something like `PP_SIGN_PARSE_PARAMS_TAG_NAMESPACE_NOT()` */ \
            seq_sign_err)

// Handle empty `()` (C99 only).
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_EMPTY_(seq_sign_err, d) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_RETURN_ERR(seq_sign_err, \
            ERROR_specified_empty_template_parameters)

// Parse params.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_D_(d, seq_sign_err) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_APPLY_EMPTY(seq_sign_err, \
            BOOST_DETAIL_PP_AUX_SIGN_PARSE_TPARAMS_, \
            BOOST_DETAIL_PP_AUX_SIGN_PARSE_TPARAMS_EMPTY_, d)

// Public API.

// Template params as `( (NIL) { ((E, E, type, name, default-value E) }* )` so
// just `( (NIL) )` if no template param.
#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_D(d, seq) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_D_(d, \
            BOOST_DETAIL_PP_SIGN_PARSE_START_NOEMPTY(seq, \
            ERROR_missing_template_parameters))

#define BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS(seq) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSE_TPARAMS_D(1, seq) 

#endif // #include guard

