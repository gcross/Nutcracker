
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_FALSE_HPP_
#define BOOST_DETAIL_PP_KEYWORD_FALSE_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_FALSE_IS_false (1) /* unary */
#define false_BOOST_DETAIL_PP_KEYWORD_FALSE_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE_false /* nothing */
#define false_BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_FALSE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_FALSE_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_FALSE_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_FALSE_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_FALSE_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_FALSE_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_FALSE_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_FALSE_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_FALSE_FRONT, false)

#define BOOST_DETAIL_PP_KEYWORD_FALSE_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_FALSE_BACK, false)

#endif // #include guard

