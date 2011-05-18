
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_THROW_HPP_
#define BOOST_DETAIL_PP_KEYWORD_THROW_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_THROW_IS_throw (1) /* unary */
#define throw_BOOST_DETAIL_PP_KEYWORD_THROW_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE_throw /* nothing */
#define throw_BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_THROW_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_THROW_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_THROW_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_THROW_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_THROW_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_THROW_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW_FRONT, throw)

#define BOOST_DETAIL_PP_KEYWORD_THROW_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_THROW_BACK, throw)

#endif // #include guard

