
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Local exits allow to program code that is executed when the enclosing
 *  scope is exited.
 */

#ifndef BOOST_LOCAL_EXIT_HPP_
#define BOOST_LOCAL_EXIT_HPP_

#include "function.hpp"
#include "aux_/exit.hpp"

/**
 * @brief This macro starts the declaration of a local exit.
 *
 * This macro must be used within a declarative context, it must be followed by
 * the local exit body code <c>{ ... }</c> and then by the
 * @Ref{BOOST_LOCAL_EXIT_END} macro (see the @Sect{Tutorial}
 * section):
 * @code
 *  { // Some declarative context.
 *      ...
 *
 *      BOOST_LOCAL_EXIT(
 *      parenthesized_binding
 *      ) exception_specifications_optional {
 *          ... // Block body.
 *      } BOOST_LOCAL_EXIT_END
 *
 *      ...
 *  }
 * @endcode
 *
 * Within templates, the special macro @Ref{BOOST_LOCAL_EXIT_TPL}
 * must be used instead of @Ref{BOOST_LOCAL_EXIT}.
 *
 * @Note A <c>return;</c> instruction from within a local exit body jumps to
 *  the end of the local exit body and it does not return the enclosing scope.
 *
 * @Params
 * @Param{parenthesized_binding,
 *  A Boost.Preprocessor sequence that uses the parenthesized syntax to specify
 *  the variables in scope to bind (see the @Sect{Grammar} section).
 * }
 * @EndParams
 *
 * As usual, exception specifications can be optionally programmed before the
 * body code block (see the @Sect{Advanced} section).
 *
 * @See @Sect{Tutorial} section, @Sect{Advanced} section, @Sect{Grammar}
 *  section, @Ref{BOOST_LOCAL_EXIT_TPL},
 *  @Ref{BOOST_LOCAL_EXIT_END}.
 */
#define BOOST_LOCAL_EXIT(parenthesized_binding) \
    BOOST_LOCAL_AUX_EXIT(BOOST_LOCAL_FUNCTION, \
            parenthesized_binding)

/**
 * @brief This macro is the same as @Ref{BOOST_LOCAL_EXIT} but
 *  it must used when declaring local exits within templates.
 *
 * @See @Ref{BOOST_LOCAL_EXIT}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_EXIT_TPL(parenthesized_binding) \
    BOOST_LOCAL_AUX_EXIT(BOOST_LOCAL_FUNCTION_TPL, \
            parenthesized_binding)
/**
 * @brief This macro ends the definition of a local exit.
 *
 * This macro must follow the local exit body code <c>{ ... }</c> as
 * shown in the @Ref{BOOST_LOCAL_EXIT} documentation.
 *
 * @See @Ref{BOOST_LOCAL_EXIT}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_EXIT_END \
    /* user code exit `{ ... }` here */ \
    /* `END_RENAME()` because `EXIT_NAME()` must append `__LINE__` */ \
    BOOST_LOCAL_FUNCTION_END_RENAME( \
            BOOST_LOCAL_AUX_EXIT_NAME(function)) \
    ::boost::local::aux::exit_guard \
            BOOST_LOCAL_AUX_EXIT_NAME(guard)( \
            BOOST_LOCAL_AUX_EXIT_NAME(function));

#endif // #include guard

