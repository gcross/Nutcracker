
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_NAMES_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_NAMES_HPP_

#include "has_const_bind.hpp"
#include "has_bind.hpp"
#include "const_bind_names.hpp"
#include "bind_names.hpp"
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/facilities/expand.hpp>

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_NAMES(params) \
    (BOOST_PP_NIL) \
    /* seq concat */ \
    BOOST_PP_IIF(BOOST_PP_EXPAND( /* expand for MSVC */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_CONST_BIND(params)), \
        BOOST_DETAIL_PP_NILSEQ_TO_SEQ \
    , \
        BOOST_PP_TUPLE_EAT(1) \
    )(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_NAMES(params)) \
    /* seq concat */ \
    BOOST_PP_IIF(BOOST_PP_EXPAND( /* expand for MSVC */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND(params)), \
        BOOST_DETAIL_PP_NILSEQ_TO_SEQ \
    , \
        BOOST_PP_TUPLE_EAT(1) \
    )(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES(params))

#endif // #include guard

