
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_HPP_
#define BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_HPP_

#include "bind.hpp"
#include <boost/detail/preprocessor/keyword/const.hpp>
#include <boost/detail/preprocessor/keyword/facility/add.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>

// These are not local macros -- DO NOT #UNDEF.
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS_bind (1) /* unary */
#define bind_BOOST_LOCAL_AUX_PP_KEYWORD_BIND_IS (1) /* unary */
#define BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_bind /* nothing */
#define bind_BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE /* nothing */

// Is.

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_FRONT_(tokens) \
    BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_FRONT( \
            BOOST_DETAIL_PP_KEYWORD_CONST_REMOVE_FRONT(tokens))

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_FRONT(tokens) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_KEYWORD_IS_CONST_FRONT(tokens), \
        BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_FRONT_ \
    , \
        0 BOOST_PP_TUPLE_EAT(1) \
    )(tokens)
        
#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_BACK_(tokens) \
    BOOST_DETAIL_PP_KEYWORD_IS_CONST_BACK( \
            BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_BACK(tokens))

#define BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONDT_BIND_BACK(tokens) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_KEYWORD_IS_BIND_BACK(tokens) \
        BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_BACK_ \
    , \
        0 BOOST_PP_TUPLE_EAT(1) \
    )(tokens)

// Rremove.

#define BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_FRONT(tokens) \
    BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_FRONT( \
            BOOST_DETAIL_PP_KEYWORD_CONST_REMOVE_FRONT(tokens))

#define BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_REMOVE_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_CONST_REMOVE_BACK( \
            BOOST_LOCAL_AUX_PP_KEYWORD_BIND_REMOVE_BACK(tokens))

// Add.

#define BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_ADD_FRONT(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_FRONT(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_FRONT, const bind)

#define BOOST_LOCAL_AUX_PP_KEYWORD_CONST_BIND_ADD_BACK(tokens) \
    BOOST_DETAIL_PP_KEYWORD_FACILITY_ADD_BACK(tokens, \
            BOOST_LOCAL_AUX_PP_KEYWORD_IS_CONST_BIND_BACK, const bind)

#endif // #include guard

