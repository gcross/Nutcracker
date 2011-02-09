
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Configuration macros allow to change the behaviour of this library
 *  at compile-time.
 */

#ifndef BOOST_LOCAL_CONFIG_HPP_
#define BOOST_LOCAL_CONFIG_HPP_

/**
 * @brief Maximum number of function parameters supported for the local
 *  functions.
 *
 * @Note If programmers leave this macro #undefined, its default value is
 * <c>5</c>.
 * 
 * This only refers to the number of local function parameters and not to the
 * number of bound variables in scope (the limit on the number of bound
 * variables is instead the maximum size allowed for a Boost.Preprocessor
 * sequences).
 *
 * @Warning Increasing this number will increase compilation time.
 *
 * @See @Sect{Starting} section.
 */
#ifndef BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX
#define BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX 5
#endif

/**
 * @brief The name of the special symbol used to access the bound object
 * <c>this</c>.
 *
 * @Note If programmers leave this macro #undefined, the default symbol used is
 * <c>this_</c>.
 *
 * @Warning Programmers should not #define this macro unless it is absolutely
 *  necessary (e.g., to avoid name clashes with another library which cannot be
 *  changed). Changing the symbol <c>this_</c> effectively changes the public
 *  API of this library.
 *
 * @See @Sect{Starting} section.
 */
#ifndef BOOST_LOCAL_CONFIG_THIS_PARAM_NAME
#define BOOST_LOCAL_CONFIG_THIS_PARAM_NAME this_
#endif

#endif // #include guard

