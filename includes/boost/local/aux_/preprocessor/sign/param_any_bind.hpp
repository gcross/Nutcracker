
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_HPP_

#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>

// PRIVATE //

// Not-this bind is 2-tuple `(name_without_type, name_with_type)`.
#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_WITHOUT_TYPE_  0
#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_WITH_TYPE_     1
#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_MAX_           2

// PUBLIC //

// Bind not-this (const or not).

#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(any_bind) \
    BOOST_PP_TUPLE_ELEM(BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_MAX_, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_WITHOUT_TYPE_, \
            any_bind)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITH_TYPE(any_bind) \
    BOOST_PP_TUPLE_ELEM(BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_MAX_, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_INDEX_WITH_TYPE_, \
            any_bind)(/* expand empty */)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_HAS_TYPE(any_bind) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITH_TYPE(any_bind)))

// Bind this (const or not).

#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_THIS_TYPE(any_bind_this_type) \
    BOOST_PP_TUPLE_ELEM(1, 0, any_bind_this_type)(/* expand empty */)

#define BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_THIS_HAS_TYPE( \
        any_bind_this_type) \
    BOOST_PP_NOT(BOOST_PP_IS_EMPTY( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_THIS_TYPE( \
            any_bind_this_type)))

#endif // #include guard

