
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_KEYWORD_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_HPP_

#include <boost/detail/preprocessor/keyword/facility/is.hpp>
#include <boost/detail/preprocessor/keyword/facility/add.hpp>
#include <boost/detail/preprocessor/keyword/facility/remove.hpp>

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS_bind (1) /* unary */
#define bind_BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS (1) /* unary */
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_bind /* nothing */
#define bind_BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE /* nothing */

// Is.

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS_)

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_IS_BACK(tokens, \
            _BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS)

// Rremove.

#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_FRONT, \
            BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_)

#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_REMOVE_BACK(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_BACK, \
            _BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE)

// Add.

#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_FRONT, bind)

#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_BACK, bind)

#endif // #include guard

