
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARSED_APPEND_UNBIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_APPEND_UNBIND_HPP_

#include "../params_unbind.hpp"
#include "../params_bind.hpp"
#include "../params_const_bind.hpp"
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/list/append.hpp>

// unbind_classified_type_and_name: [auto | register] type name
#define BOOST_LOCAL_AUX_PP_SIGN_PARSED_PARAMS_APPEND_UNBIND( \
        params, unbind_classified_type_and_name) \
    ( /* unbind params and defaults */ \
        BOOST_PP_LIST_APPEND(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(params), \
                /* append param (with no default -- EMPTY) */ \
                ((unbind_classified_type_and_name, BOOST_PP_EMPTY), \
                BOOST_PP_NIL)) \
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

