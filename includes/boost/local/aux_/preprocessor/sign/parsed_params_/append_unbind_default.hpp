
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSED_APPEND_UNBIND_DEFAULT_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_APPEND_UNBIND_DEFAULT_HPP_

#include "../params_unbind.hpp"
#include "../params_bind.hpp"
#include "../params_const_bind.hpp"
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/list/size.hpp>
#include <boost/preprocessor/list/at.hpp>
#include <boost/preprocessor/list/append.hpp>
#include <boost/preprocessor/list/first_n.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_APPEND_UNBIND_DEFAULT_( \
        unbinds, default_value) \
    /* `DEC` ok because precondition that unbinds are not nil-list */ \
    BOOST_PP_LIST_APPEND( \
        BOOST_PP_LIST_FIRST_N(BOOST_PP_DEC(BOOST_PP_LIST_SIZE(unbinds)), \
                unbinds) \
    , \
        ( /* list 2-tuple */ \
            ( /* (param_decl, default) 2-tuple */ \
                BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_PARAM_DECL( \
                        BOOST_PP_LIST_AT(unbinds, BOOST_PP_DEC( \
                                BOOST_PP_LIST_SIZE(unbinds)))) \
            , \
                default_value BOOST_PP_EMPTY \
            ) \
        , \
            BOOST_PP_NIL \
        ) \
    )

// PUBLIC //

// default_value: a valid parameter default value (`-1`, etc)
// Precondition: already added unbinds are not nil-list.
#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_APPEND_UNBIND_DEFAULT( \
        params, default_value) \
    ( /* unbind params and defaults */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_APPEND_UNBIND_DEFAULT_( \
                BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(params), \
                default_value) /* append default to last added param */ \
    , /* const-bind names */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND(params) \
    , /* const-bind `this` types */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_THIS_TYPE(params) \
    , /* bind names */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND(params) \
    , /* bind `this` types */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_THIS_TYPE(params) \
    , /* error message (if any) */ \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR(params) \
    )

#endif // #include guard

