
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_USING_HPP_
#define BOOST_DETAIL_PP_KEYWORD_USING_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_USING_IS_using (1) /* unary */
#define using_BOOST_DETAIL_PP_KEYWORD_USING_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_USING_REMOVE_using /* nothing */
#define using_BOOST_DETAIL_PP_KEYWORD_USING_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_USING_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_USING_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_USING_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_USING_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_USING_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_USING_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_USING_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_USING_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_USING_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_USING_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_USING_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_USING_FRONT, using)

#define BOOST_DETAIL_PP_KEYWORD_USING_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_USING_BACK, using)

#endif // #include guard

