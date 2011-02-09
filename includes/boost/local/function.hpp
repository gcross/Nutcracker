
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Local functions allow to program functions locally within the scope
 *  where they are needed.
 */

#ifndef BOOST_LOCAL_FUNCTION_HPP_
#define BOOST_LOCAL_FUNCTION_HPP_

#include "aux_/function/decl.hpp"
#include "aux_/function/end.hpp"

/**
 * @brief This macro starts the declaration of a local function.
 *
 * This macro must be used within a declarative context, it must be followed by
 * the local function body code <c>{ ... }</c> and then by either the
 * @Ref{BOOST_LOCAL_FUNCTION_END} or the
 * @Ref{BOOST_LOCAL_FUNCTION_END_RENAME} macro (see the
 * @Sect{Tutorial} and @Sect{Advanced} sections):
 * @code
 *  { // Some declarative context.
 *      ...
 *
 *      BOOST_LOCAL_FUNCTION(
 *      parenthesized_signature
 *      ) exception_specifications_optional {
 *          ... // Block body.
 *      } BOOST_LOCAL_FUNCTION_END(local_function_name)
 *
 *      ...
 *  }
 * @endcode
 *
 * Within templates, the special macro
 * @Ref{BOOST_LOCAL_FUNCTION_TPL} must be used instead of
 * @Ref{BOOST_LOCAL_FUNCTION}.
 *
 * @Params
 * @Param{parenthesized_signature,
 *  A Boost.Preprocessor sequence that uses the parenthesized syntax to specify
 *  the local function signature and the variables in scope to bind (see the
 *  @Sect{Grammar} section).
 * }
 * @EndParams
 *
 * As usual, exception specifications can be optionally programmed before the
 * body code block (see the @Sect{Advanced} section).
 *
 * @See @Sect{Tutorial} section, @Sect{Advanced} section, @Sect{Grammar}
 *  section, @Ref{BOOST_LOCAL_FUNCTION_TPL},
 *  @Ref{BOOST_LOCAL_FUNCTION_END},
 *  @Ref{BOOST_LOCAL_FUNCTION_END_RENAME}.
 */
#define BOOST_LOCAL_FUNCTION(parenthesized_signature) \
    BOOST_LOCAL_AUX_FUNCTION_DECL(parenthesized_signature, \
            BOOST_PP_EMPTY() /* no typename keyword */)
/**
 * @brief This macro is the same as @Ref{BOOST_LOCAL_FUNCTION} but
 *  it must be used when declaring local functions within templates.
 *
 * @See @Ref{BOOST_LOCAL_FUNCTION}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_FUNCTION_TPL(parenthesized_signature) \
    BOOST_LOCAL_AUX_FUNCTION_DECL(parenthesized_signature, \
            typename)

/**
 * @brief This macro ends the definition of a local function.
 *
 * This macro must follow the local function body code <c>{ ... }</c> as
 * shown in the @Ref{BOOST_LOCAL_FUNCTION} documentation.
 *
 * @Params
 * @Param{local_function_name,
 *  The name of the local function as it has been specified by the
 *  parenthesized signature passed to @Ref{BOOST_LOCAL_FUNCTION}
 *  (otherwise\, the compiler will generate an error).
 *  See the @Ref{BOOST_LOCAL_FUNCTION_END_RENAME} macro to relax
 *  this constraint.
 * }
 * @EndParams
 *
 * @See @Ref{BOOST_LOCAL_FUNCTION_END_RENAME},
 *  @Ref{BOOST_LOCAL_FUNCTION}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_FUNCTION_END(local_function_name) \
    BOOST_LOCAL_AUX_FUNCTION_END(local_function_name)

/**
 * @brief This macro ends the definition of a local function while changing
 *  the function name.
 *
 * This macro must be used instead of the
 * @Ref{BOOST_LOCAL_FUNCTION_END} macro and it must follow the
 * local function body code <c>{ ... }</c> as shown in the
 * @Ref{BOOST_LOCAL_FUNCTION} documentation.
 *
 * @Params
 * @Param{new_local_function_name,
 *  This is the new local function name (see the @Sect{Advanced} section).
 * }
 * @EndParams
 *
 * The original local function name (as specified by the parenthesized
 * signature passed to @Ref{BOOST_LOCAL_FUNCTION}) must still be
 * used within the local function body (e.g., for the local function to
 * recursively call itself).
 * However, within the enclosing scope the new local function as passed to
 * @Ref{BOOST_LOCAL_FUNCTION_END_RENAME} must be used (e.g., to
 * invoke the local function).
 *
 * @Warning Renamed local functions cannot be directly passed as template
 *  parameters (e.g., to STL algorithms).
 *  However, it is possible to workaround this limitation by getting a
 *  reference to the renamed local function using
 *  boost::local::@NamespaceRef{boost/local/,function_ref}
 *  and then by passing the local function reference as template parameter.
 *
 * @See @Sect{Advanced} section,
 *  @Ref{BOOST_LOCAL_FUNCTION_END},
 *  @Ref{BOOST_LOCAL_FUNCTION},
 *  boost::local::@NamespaceRef{boost/local/,function_ref}.
 */
#define BOOST_LOCAL_FUNCTION_END_RENAME(new_local_function_name) \
    BOOST_LOCAL_AUX_FUNCTION_END_RENAME(new_local_function_name)

#endif // #include guard

