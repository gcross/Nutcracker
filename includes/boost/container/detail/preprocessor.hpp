//////////////////////////////////////////////////////////////////////////////
//
// (C) Copyright Ion Gaztanaga 2008-2009. Distributed under the Boost
// Software License, Version 1.0. (See accompanying file
// LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
// See http://www.boost.org/libs/container for documentation.
//
//////////////////////////////////////////////////////////////////////////////

#ifndef BOOST_CONTAINERS_DETAIL_PREPROCESSOR_HPP
#define BOOST_CONTAINERS_DETAIL_PREPROCESSOR_HPP

#if (defined _MSC_VER) && (_MSC_VER >= 1200)
#  pragma once
#endif

#include "config_begin.hpp"
#include INCLUDE_BOOST_CONTAINER_DETAIL_WORKAROUND_HPP

#ifdef BOOST_CONTAINERS_PERFECT_FORWARDING
#error "This file is not needed when perfect forwarding is available"
#endif

#include <boost/preprocessor/iteration/local.hpp> 
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>

#define BOOST_CONTAINERS_MAX_CONSTRUCTOR_PARAMETERS 10

//Note:
//We define template parameters as const references to
//be able to bind temporaries. After that we will un-const them.
//This cast is ugly but it is necessary until "perfect forwarding"
//is achieved in C++0x. Meanwhile, if we want to be able to
//bind rvalues with non-const references, we have to be ugly
#ifndef BOOST_NO_RVALUE_REFERENCES
   #define BOOST_CONTAINERS_PP_PARAM_LIST(z, n, data) \
   BOOST_PP_CAT(P, n) && BOOST_PP_CAT(p, n) \
   //!
#else
   #define BOOST_CONTAINERS_PP_PARAM_LIST(z, n, data) \
   const BOOST_PP_CAT(P, n) & BOOST_PP_CAT(p, n) \
   //!
#endif

#ifndef BOOST_NO_RVALUE_REFERENCES
   #define BOOST_CONTAINERS_PARAM(U, u) \
   U && u \
   //!
#else
   #define BOOST_CONTAINERS_PARAM(U, u) \
   const U & u \
   //!
#endif

#ifndef BOOST_NO_RVALUE_REFERENCES
#define BOOST_CONTAINERS_AUX_PARAM_INIT(z, n, data) \
  BOOST_PP_CAT(m_p, n) (BOOST_PP_CAT(p, n))           \
//!
#else
#define BOOST_CONTAINERS_AUX_PARAM_INIT(z, n, data) \
  BOOST_PP_CAT(m_p, n) (const_cast<BOOST_PP_CAT(P, n) &>(BOOST_PP_CAT(p, n))) \
//!
#endif

#define BOOST_CONTAINERS_AUX_PARAM_INC(z, n, data)   \
  BOOST_PP_CAT(++m_p, n)                        \
//!

#ifndef BOOST_NO_RVALUE_REFERENCES
#define BOOST_CONTAINERS_AUX_PARAM_DEFINE(z, n, data)  \
  BOOST_PP_CAT(P, n) && BOOST_PP_CAT(m_p, n);            \
//!
#else
#define BOOST_CONTAINERS_AUX_PARAM_DEFINE(z, n, data)  \
  BOOST_PP_CAT(P, n) & BOOST_PP_CAT(m_p, n);             \
//!
#endif

#define BOOST_CONTAINERS_PP_PARAM_FORWARD(z, n, data) \
BOOST_CONTAINER_MOVE_NAMESPACE::forward< BOOST_PP_CAT(P, n) >( BOOST_PP_CAT(p, n) ) \
//!

#define BOOST_CONTAINERS_PP_MEMBER_FORWARD(z, n, data) \
BOOST_CONTAINER_MOVE_NAMESPACE::forward< BOOST_PP_CAT(P, n) >( BOOST_PP_CAT(m_p, n) ) \
//!

#define BOOST_CONTAINERS_PP_MEMBER_IT_FORWARD(z, n, data) \
BOOST_PP_CAT(*m_p, n) \
//!

#include INCLUDE_BOOST_CONTAINER_DETAIL_CONFIG_END_HPP

#else
#ifdef BOOST_CONTAINERS_PERFECT_FORWARDING
#error "This file is not needed when perfect forwarding is available"
#endif
#endif //#ifndef BOOST_CONTAINERS_DETAIL_PREPROCESSOR_HPP