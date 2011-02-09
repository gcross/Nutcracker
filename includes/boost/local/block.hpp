
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Local blocks allow to program code that is executed accessing some of
 * the variables in scope as constants.
 */

#ifndef BOOST_LOCAL_BLOCK_HPP_
#define BOOST_LOCAL_BLOCK_HPP_

#include "function.hpp"
#include "aux_/block.hpp"
#include "aux_/symbol.hpp"

/**
 * @brief This macro starts the declaration of a local block.
 *
 * This macro must be used within a declarative context, it must be followed by
 * the local block body code <c>{ ... }</c> and then by the
 * @Ref{BOOST_LOCAL_BLOCK_END} macro (see the Tutorial section):
 * @code
 *  { // Some declarative context.
 *      ...
 *
 *      BOOST_LOCAL_BLOCK(
 *      parenthesized_binding
 *      ) exception_specifications_optional {
 *          ... // Block body.
 *      } BOOST_LOCAL_BLOCK_END
 *
 *      ...
 *  }
 * @endcode
 *
 * Within templates, the special macro @Ref{BOOST_LOCAL_BLOCK_TPL}
 * must be used instead of @Ref{BOOST_LOCAL_BLOCK}.
 *
 * @Note A <c>return;</c> instruction from within a local block body jumps to
 *  the end of the local block body and it does not return the enclosing scope.
 *
 * @Params
 * @Param{parenthesized_binding,
 *  A Boost.Preprocessor sequence that uses the parenthesized syntax to specify
 *  the variables in scope to bind (see the Grammar section).
 * }
 * @EndParams
 *
 * As usual, exception specifications can be optionally programmed before the
 * body code block (see the Advanced section).
 *
 * @See @Sect{Tutorial} section, @Sect{Advanced} section, @Sect{Grammar}
 *  section, @Ref{BOOST_LOCAL_BLOCK_TPL},
 *  @Ref{BOOST_LOCAL_BLOCK_END}.
 */
#define BOOST_LOCAL_BLOCK(parenthesized_binding) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION, \
            parenthesized_binding)

/**
 * @brief This macro is the same as @Ref{BOOST_LOCAL_BLOCK} but
 *  it must be used when declaring local blocks within templates.
 *
 * @See @Ref{BOOST_LOCAL_BLOCK}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_BLOCK_TPL(parenthesized_binding) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION_TPL, \
            parenthesized_binding)

/**
 * @brief This macro ends the definition of a local block.
 *
 * This macro must follow the local block body code <c>{ ... }</c> as
 * shown in the @Ref{BOOST_LOCAL_BLOCK} documentation.
 *
 * @See @Ref{BOOST_LOCAL_BLOCK}, @Sect{Tutorial} section.
 */
#define BOOST_LOCAL_BLOCK_END \
        /* user block code `{ ... }` here */ \
        /* ends the local function declaration */ \
        BOOST_LOCAL_FUNCTION_END_RENAME( \
                BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME) \
        /* call local funciton and execute block's code imediately */ \
        BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME(); \
    }

#endif // #include guard

