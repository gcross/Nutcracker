
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_IS_VARIADIC_HPP_
#define BOOST_LOCAL_AUX_PP_IS_VARIADIC_HPP_

#include <boost/config.hpp>

#if !defined(BOOST_NO_VARIADIC_MACROS) // If no variadics then no macros.

#include "size.hpp"
#include "eat.hpp"
#include <boost/preprocessor/logical/not.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/comparison/greater.hpp>
// `IS_UNARY` not working on Borland and other pp which have no variadic anyway.
#include <boost/preprocessor/detail/is_unary.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_IS_VARIADIC_NOT_UNARY_(...) \
    BOOST_PP_NOT(BOOST_PP_IS_UNARY(__VA_ARGS__))

// PUBLIC //

#define BOOST_LOCAL_AUX_PP_IS_VARIADIC(...) \
    BOOST_PP_IF(BOOST_PP_GREATER( /* IIF does not expand on MSVC */ \
            BOOST_LOCAL_AUX_PP_VARIADIC_SIZE(__VA_ARGS__), 1), \
        1 BOOST_LOCAL_AUX_PP_VARIADIC_EAT \
    , \
        BOOST_LOCAL_AUX_PP_IS_VARIADIC_NOT_UNARY_ \
    )(__VA_ARGS__)

#endif // BOOST_NO_VARIADIC_MACROS

#endif // #include guard


