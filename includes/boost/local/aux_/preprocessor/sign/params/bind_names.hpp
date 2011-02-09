
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES_HPP_

#include "has_bind.hpp"
#include <boost/detail/preprocessor/sign/params/size.hpp>
#include <boost/detail/preprocessor/sign/param.hpp>
#include <boost/detail/preprocessor/sign/param/name.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// Has `bind` so params size >= 1 and `bind` is last param.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_GET_LAST_(params) \
    (BOOST_PP_NIL) /* seq concat */ \
    BOOST_DETAIL_PP_SIGN_PARAM_NAME(BOOST_DETAIL_PP_SIGN_PARAM( \
            BOOST_PP_DEC(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params)), params))

// Return bind names as nil-seq.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES(params) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND(params), \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_GET_LAST_ \
    , \
        (BOOST_PP_NIL) /* no bind param name */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(params)

#endif // #include guard

