
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_NAMESPACE_HPP_
#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_IS_namespace (1) /* unary */
#define namespace_BOOST_DETAIL_PP_KEYWORD_NAMESPACE_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE_namespace /* nothing */
#define namespace_BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_NAMESPACE_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_NAMESPACE_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_NAMESPACE_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_FRONT, namespace)

#define BOOST_DETAIL_PP_KEYWORD_NAMESPACE_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_NAMESPACE_BACK, namespace)

#endif // #include guard

