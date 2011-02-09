
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Identity macros are used to pass expressions with commas (not already
 *  wrapped within round parenthesis) as macros parameters.
 *
 * @Note The only way (at least as far as the authors know) to pass an
 *  arbitrary number of commas within macro parameters without explicitly
 *  indicating the number of commas is to wrap the parameter within round
 *  parenthesis that are not replaced by macro expansion and then to remove the
 *  extra parenthesis at compile-time (for type expressions) or at run-time
 *  (for value expressions).
 *  This is what these macros do.
 */

#ifndef BOOST_IDENTITY_HPP_
#define BOOST_IDENTITY_HPP_

#include "aux_/identity.hpp"

/**
 * @brief This macro wraps the specified type expression within extra
 *  parenthesis so the type can be passed as a single macro parameter even if
 *  it contains commas.
 *
 * This macro expands to an expression that can be passed as a single macro
 * parameter even if it contains commas and that evaluates to the specified
 * type at compile-time (see the @Sect{Advanced} section).
 *
 * For example <c>BOOST_IDENTITY_TYPE((std::map<int, double>))</c> can be
 * passed as a single macro parameter when instead
 * <c>std::map<int, double></c> cannot (because it contains a comma not wrapped
 * by round parenthesis so it will be interpreted as two separate macro
 * parameters by the preprocessor).
 *
 * @Params
 * @Param{parenthesized_type,
 *  The type expression to be passed as macro parameter wrapped by a single set
 *  of round parenthesis <c>(...)</c>.
 *  This type expression can contain an arbitrary number of commas.
 * }
 * @EndParams
 *
 * @See @Sect{Advanced} section.
 */
#define BOOST_IDENTITY_TYPE(parenthesized_type) \
    /* must NOT prefix this with `::` to work with parenthesized syntax */ \
    boost::aux::identity_type< void parenthesized_type >::type

/**
 * @brief This macro wraps the specified value expression within extra
 *  parenthesis so the value can be passed as a single macro parameter even if
 *  it contains commas.
 *
 * This macro expands to an expression that can be passed as a single macro
 * parameter even if it contains commas and that evaluates to the specified
 * value at run-time (see the @Sect{Advanced} section).
 *
 * For example <c>BOOST_IDENTITY_VALUE((key_size<int, double>::value))</c>
 * can be passed as a single macro parameter when instead
 * <c>key_size<int, double>::value</c> cannot (because it contains a comma not
 * wrapped by round parenthesis so it will be interpreted as two separate macro
 * parameters by the preprocessor).
 *
 * @Params
 * @Param{parenthesize_value,
 *  The value expression to be passed as macro parameter wrapped by a single
 *  set of round parenthesis <c>(...)</c>.
 *  This value expression can contain an arbitrary number of commas.
 * }
 * @EndParams
 *
 * @See @Sect{Advanced} section.
 */
#define BOOST_IDENTITY_VALUE(parenthesized_value) \
    /* must NOT prefix this with `::` to work with parenthesized syntax */ \
    boost::aux::identity_value parenthesized_value

#endif // #include guard

