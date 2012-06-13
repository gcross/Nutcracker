
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Local blocks allow to program code that is executed "in place" while
 *  accessing some of the variables in scope as constants.
 */

#ifndef BOOST_LOCAL_BLOCK_HPP_
#define BOOST_LOCAL_BLOCK_HPP_

#include "config.hpp"
#include "function.hpp"
#include "aux_/block.hpp"
#include <boost/config.hpp> // For variadic macros.

#if defined(BOOST_NO_VARIADIC_MACROS) || defined(BOOST_LOCAL_CONFIG_COMPLIANT)

/**
 * @brief This macro starts the declaration of a local block.
 *
 * This macro must be used within a declarative context, it must be followed by
 * the local block body code and then by the @RefMacro{BOOST_LOCAL_BLOCK_END}
 * macro (see the @RefSect{Tutorial} section):
 * @code
 *  { // Some declarative context.
 *      ...
 *      BOOST_LOCAL_BLOCK(bindings) {
 *          ... // Body code.
 *      } BOOST_LOCAL_BLOCK_END
 *      ...
 *  }
 * @endcode
 *
 * As usual, exceptions specifications can be optionally programmed before the
 * body code block (but the specifications will only apply the body code and
 * not to the library code automatically generated by the macro expansion,
 * see @RefSect2{Advanced_Topics, Advanced Topics}).
 *
 * Within templates, the special macro @RefMacro{BOOST_LOCAL_BLOCK_TPL} must be
 * used instead of @RefMacro{BOOST_LOCAL_BLOCK}.
 *
 * @Note A <c>return;</c> instruction from within a local block body jumps to
 *  the end of the local block body and it does not return the enclosing scope.
 *
 * @Params
 * @Param{bindings,
 *  On all C++ compilers\, the <em>sequencing macro syntax</em> defined by the
 *  following grammar is used to specify the variables in scope to bind:
 *  @code
 *      bindings:
 *              void_list | bound_list
 *      void_list:
 *              (void)
 *      bound_list:
 *              (bound_parameter1) (bound_parameter2) ...
 *      bound_parameter:
 *              [const] bind [(variable_type)] [&] variable_name
 *  @endcode
 *  Where the following lexical conventions have been used:
 *  <c>token1 | token2</c> means either <c>token1</c> or <c>token2</c>;
 *  <c>[token]</c> means either <c>token</c> or nothing (i.e.\, <c>token</c> is
 *  optional). 
 *
 *  On C99 and later compilers that support variadic macros\, the above
 *  grammar can be modified as follow to define the <em>variadic macro
 *  syntax</em> that can also be used to specify the variables in scope to
 *  bind:
 *  @code
 *      void_list:
 *              void
 *      bound_list:
 *              bound_parameter1\, bound_parameter2\, ...
 *  @endcode
 *  Note that the variadic macro syntax uses commas <c>\,</c> instead of
 *  parenthesis <c>()</c> to separate the parameters and therefore resembles
 *  the usual C++ function parameter declaration syntax more closely than the
 *  sequencing macro syntax. However\, the variadic macro syntax will only work
 *  on C++ compilers that support variadic macros so it should be used with
 *  care in order to avoid portability issues. 
 *
 *  Finally\, on C++ compilers that support empty macro parameters\, the above
 *  grammars can be modified as follow to define the <em>empty macro
 *  syntax</em> that can also be used to specify an empty list of variables to
 *  bind:
 *  @code
 *      void_list:
 *              ()
 *  @endcode
 *  Note that the empty macro parameter syntax uses the usual C++ syntax to
 *  specify empty bound variable list but it will only work on compilers
 *  that support empty macro parameters so it should be used with care in order
 *  to avoid portability issues.
 * }
 * @EndParams
 * 
 * When the object <c>this</c> is specified as the name of the variable to
 * bind, the body must use the special name <c>this_</c> to access the bound
 * object.
 * The special name <c>this_</c> is defined by the configuration macro
 * @RefMacro{BOOST_LOCAL_CONFIG_THIS_PARAM_NAME}.
 *
 * Both the variadic and empty macro syntaxes are always disabled when the
 * configuration macro @RefMacro{BOOST_LOCAL_CONFIG_COMPLIANT} is defined.
 *
 * @Note This macro cannot be used multiple times on the same line because it
 *  internally uses the line number <c>__LINE__</c> to generate unique
 *  identifiers.
 *
 * @See @RefSect{Tutorial} section, @RefSect2{Advanced_Topics, Advanced Topics}
 *  section, @RefMacro{BOOST_LOCAL_BLOCK_TPL},
 *  @RefMacro{BOOST_LOCAL_BLOCK_END},
 *  @RefMacro{BOOST_LOCAL_CONFIG_THIS_PARAM_NAME},
 *  @RefMacro{BOOST_LOCAL_CONFIG_COMPLIANT}.
 */
#define BOOST_LOCAL_BLOCK(bindings) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION_PARAMS, bindings)
/**
 * @brief This macro is the same as @RefMacro{BOOST_LOCAL_BLOCK} but
 *  it must be used when declaring local blocks within templates.
 *
 * @See @RefMacro{BOOST_LOCAL_BLOCK}, @RefSect{Tutorial} section.
 */
#define BOOST_LOCAL_BLOCK_TPL(bindings) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION_PARAMS_TPL, bindings)

#else // BOOST_NO_VARIADIC_MACROS

#include "aux_/preprocessor/variadic/to_seq.hpp"

#define BOOST_LOCAL_BLOCK(...) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION_PARAMS, \
            BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ( \
                    (void) /* for empty seq */, __VA_ARGS__))

#define BOOST_LOCAL_BLOCK_TPL(...) \
    BOOST_LOCAL_AUX_BLOCK(BOOST_LOCAL_FUNCTION_PARAMS_TPL, \
            BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ( \
                    (void) /* for empty seq */, __VA_ARGS__))

#endif // BOOST_NO_VARIADIC_MACROS

/**
 * @brief This macro ends the definition of a local block.
 *
 * This macro must follow the local block body code <c>{ ... }</c> as
 * shown in the @RefMacro{BOOST_LOCAL_BLOCK} documentation.
 *
 * @Note This macro cannot be used multiple times on the same line because it
 *  internally uses the line number <c>__LINE__</c> to generate unique
 *  identifiers.
 *
 * @See @RefMacro{BOOST_LOCAL_BLOCK}, @RefSect{Tutorial} section.
 */
#define BOOST_LOCAL_BLOCK_END \
    BOOST_LOCAL_AUX_BLOCK_END(__LINE__) 

#endif // #include guard
