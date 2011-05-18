
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_HPP_
#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_HPP_

#include <boost/detail/preprocessor/keyword/facility/is.hpp>
#include <boost/detail/preprocessor/keyword/facility/add.hpp>
#include <boost/detail/preprocessor/keyword/facility/remove.hpp>

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_IS_recursive (1) /* unary */
#define recursive_BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_IS (1) /* unary */
#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE_recursive /* nothing */
#define recursive_BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE /* nothing */

// Is.

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_IS_)

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(tokens, \
            _BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_IS)

// Rremove.

#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_FRONT, \
            BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE_)

#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_BACK, \
            _BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_REMOVE)

// Add.

#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_FRONT, recursive)

#define BOOST_LOCAL_AUX_PP_KEYWORD_RECURSIVE_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_RECURSIVE_BACK, recursive)

#endif // #include guard

