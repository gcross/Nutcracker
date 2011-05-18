
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_ANY_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_ANY_BIND_HPP_

#include "params_const_bind.hpp"
#include "params_bind.hpp"
#include <boost/preprocessor/logical/bitor.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/list/append.hpp>

// Expand to 1 iff `this` is bound (const or not).
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ANY_BIND_THIS(params) \
    /* can't use PP_BITOR/PP_OR because don't expand on MSVC */ \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_CONST_BIND_THIS(params), \
        1 \
    , BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_BIND_THIS(params), \
        1 \
    , \
        0 \
    ))

// Expand to 1 iff has one or more bound including `this` (const or not).
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ANY_BIND(params) \
    /* can't use PP_BITOR/PP_OR because don't expand on MSVC */ \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_CONST_BIND(params), \
        1 \
    , BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_CONST_BIND_THIS( \
            params), \
        1 \
    , BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_BIND(params), \
        1 \
    , BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_BIND_THIS(params), \
        1 \
    , \
        0 \
    ))))

// Expand to list with all binds (const or not) but excluding `this`.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_WITHOUT_THIS(params) \
    BOOST_PP_LIST_APPEND(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND(params), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND(params))

// Expand to list with all binds (const or not) and including `this` if bound
// (const or not).
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND(params) \
    BOOST_PP_LIST_APPEND(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND(params), \
        BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ANY_BIND_THIS( \
                params), \
            (this, BOOST_PP_NIL) /* `this` never by ref because in C++ */ \
        , \
            BOOST_PP_NIL \
        ) \
    )

// Expand to list of all `this` types (const or not). There is at most 1 bind
// `this` type if no parsing error.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_TYPE(params) \
    BOOST_PP_LIST_APPEND( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_THIS_TYPE(params), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_THIS_TYPE(params))

#endif // #include guard

