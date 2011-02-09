
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE_HPP_
#define BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE_HPP_

#include "size.hpp"
#include "../aux_/params_indexes.hpp"
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/array/elem.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/facilities/expand.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// Namespace tokens can be one of the following:
// 1) `PP_EMPTY` for no namespace.
// 2) `PP_EMPTY PP_EMPTY` for default namespace.
// 3) `token PP_EMPTY PP_EMPTY` for namespace `token` (non default).
    
#define BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE_(params) \
    BOOST_PP_ARRAY_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_PARAMS_TAG_NAMESPACE_INDEX, params)

#define BOOST_DETAIL_PP_SIGN_PARAMS_HAVE_TAG_NAMESPACE_(params) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY( \
            BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE_(params) \
            (/* exapnd empty */) ))

#define BOOST_DETAIL_PP_SIGN_PARAMS_HAVE_TAG_NAMESPACE(params) \
    BOOST_PP_IF(BOOST_DETAIL_PP_SIGN_ALL_PARAMS_SIZE(params), \
        BOOST_DETAIL_PP_SIGN_PARAMS_HAVE_TAG_NAMESPACE_ \
    , \
        0 /* no params so no named params */ \
        BOOST_PP_TUPLE_EAT(1) \
    )(params)

#define BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE(params) \
    BOOST_PP_EXPAND( /* for double `PP_EMPTY` expansion */ \
            BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE_(params) \
            (/* exapnd empty */)(/* exapnd empty */) )
            
#define BOOST_DETAIL_PP_SIGN_PARAMS_HAVE_DEFAULT_TAG_NAMESPACE(params) \
    BOOST_PP_IS_EMPTY(BOOST_DETAIL_PP_SIGN_PARAMS_TAG_NAMESPACE(params))

#endif // #include guard

