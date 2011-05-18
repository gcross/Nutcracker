
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_KEYWORD_TEMPLATE_HPP_
#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_HPP_

#include "facility/is.hpp"
#include "facility/add.hpp"
#include "facility/remove.hpp"

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_IS_template (1) /* unary */
#define template_BOOST_DETAIL_PP_KEYWORD_TEMPLATE_IS (1) /* unary */
#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE_template /* nothing */
#define template_BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE /* nothing */

// Is.

#define BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_TEMPLATE_IS_)

#define BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_BACK(token) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(token, \
            _BOOST_DETAIL_PP_KEYWORD_TEMPLATE_IS)

// Rremove.

#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_FRONT, \
            BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE_)

#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_BACK, \
            _BOOST_DETAIL_PP_KEYWORD_TEMPLATE_REMOVE)

// Add.

#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_FRONT, template)

#define BOOST_DETAIL_PP_KEYWORD_TEMPLATE_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_DETAIL_PP_KEYWORD_IS_TEMPLATE_BACK, template)

#endif // #include guard

