
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_HPP_

#include "has_const_bind.hpp"
#include "has_bind.hpp"
#include <boost/detail/preprocessor/sign/params/size.hpp>
#include <boost/detail/preprocessor/sign/params/first_n.hpp>
#include <boost/preprocessor/array/elem.hpp>
#include <boost/preprocessor/seq/first_n.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/arithmetic/add.hpp>
#include <boost/preprocessor/arithmetic/sub.hpp>
#include <boost/preprocessor/control/if.hpp>
            
#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_STRIP_BINDS_( \
        params, count_binds) \
    BOOST_DETAIL_PP_SIGN_PARAMS_FIRST_N(BOOST_PP_SUB( \
            BOOST_DETAIL_PP_SIGN_PARAMS_SIZE(params), count_binds), params)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_STRIP_NOTHING_( \
        params, count_binds) \
    params /* strip nothing because there in no bind param */

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_( \
        params, count_binds) \
    BOOST_PP_IF(count_binds, \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_STRIP_BINDS_ \
    , \
        BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_STRIP_NOTHING_ \
    )(params, count_binds)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_COUNT_BINDS_(params) \
    /* 2 if both `const bind` and `bind`; 1 if either `const bind` or */ \
    /* `bind`; 0 if neihert `const bind` nor `bind` */ \
    BOOST_PP_ADD(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_CONST_BIND( \
            params), BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND( \
            params))

// Public API.

#define BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(params) \
    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_(params, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_COUNT_BINDS_( \
            params))

#endif // #include guard

