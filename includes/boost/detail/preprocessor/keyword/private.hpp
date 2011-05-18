
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_PRIVATE_HPP_
#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_IS_private (1) /* unary */
#define private_BOOST_DETAIL_PP_KEYWORD_PRIVATE_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE_private /* nothing */
#define private_BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_PRIVATE_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_PRIVATE_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_PRIVATE_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_FRONT, private)

#define BOOST_DETAIL_PP_KEYWORD_PRIVATE_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_PRIVATE_BACK, private)

#endif // #include guard

