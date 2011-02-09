
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Local function references can be passed as function and template
 *  parameters, they can be assigned to variables, etc.
 */

#ifndef BOOST_LOCAL_FUNCTION_REF_HPP_
#define BOOST_LOCAL_FUNCTION_REF_HPP_

namespace boost { namespace local {

/**
 * @brief Template to hold a reference to a local function while supporting
 *  eventual default function parameters.
 *
 * This template defines several specializations to handle a generic number
 * <c>N</c> of function parameters some of which can have default values.
 * The number of supported function parameters goes from <c>0</c> (for a
 * function with no parameter) to a maximum of
 * @NamespaceRef{../../,BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX}.
 *
 * Each template specialization defines call operators <c>operator()(...)</c>
 * with a different set of parameters to handle the number of default
 * parameters specified by <c>defaults</c> (see @NamespaceSect{../../,Advanced}
 * section):
 * @code
 *  template<typename Result, typename Arg1, ..., typename ArgN, size_t defaults = 0>
 *  class function_ref<Result (Arg1, ..., ArgN), defaults> {
 *  public:
 *      Result operator()(Arg1, ..., ArgN-2, ArgN-1, ArgN);
 *      // Call operators to handle default parameters:
 *      Result operator()(Arg1, ..., ArgN-2, ArgN-1);       // iff defaults >= 1
 *      Result operator()(Arg1, ..., ArgN-2);               // iff defaults >= 2
 *      ...                                                 // etc
 *
 *      // Copy constructor and assignment operator for local functions:
 *      function_ref(local_function<F, defaults>& ref);
 *      function_ref& operator=(local_function<F, defaults>& ref);
 *  };
 * @endcode
 * Where:
 * - <c>Result</c> is the function result type.
 *   It can be <c>void</c>.
 * - <c>ArgN</c> is the last function parameter type, <c>ArgN-1</c> is the
 *   second last function parameter type, etc.
 *   These are all optional (none is specified for a function with no
 *   parameter, only <c>Arg1</c> is specified for a function with only one
 *   parameter, etc).
 * - The operator <c>Result operator()(Arg1, ..., ArgN-2, ArgN-1)</c> is
 *   defined if and only if there are one or more default parameters
 *   (<c>defaults >= 1</c>), the operator
 *   <c>Result operator()(Arg1, ..., ArgN-2)</c> is defined if and only if
 *   there are two or more default parameters (<c>defaults >= 2</c>), etc.
 * - <c>local_function<F, defaults></c> is an internal type for a local
 *   function with a signature matching <c>F</c> and with a number of default
 *   parameters equal to <c>defaults</c>.
 * - The copy constructor and assignment operator <c>operator=</c> allow to
 *   assign this template to a reference to a local function with a signature
 *   matching <c>F</c> and with a number of default parameters equal to
 *   <c>defaults</c>.
 * 
 * @Warning Programmers must make sure that the local function survives the
 *  scope of the function reference (otherwise the reference will be invalid
 *  and its use will generate a run-time error as usual with C++ references).
 *
 * @Params
 * @Param{F,
 *  The function signature expressed using the Boost.Function's
 *  preferred syntax: <c>F = Result (Arg1\, ...\, ArgN)</c>.
 * }
 * @Param{defaults,
 *  The number of the function default parameters in
 *  <c>[0\,</c> @NamespaceRef{../../\,BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX}<c>]</c>.
 *  As usual in C++\, default parameters are counted starting from the last
 *  parameter:
 *  <c>defaults = 0</c> means that no parameter is optional\,
 *  <c>defaults = 1</c> means that the last parameter is optional\,
 *  <c>defaults = 2</c> means that the last two parameters are optional\, etc.
 * }
 * @EndParams
 *
 * @Note This template is similar to <c>boost::function<></c> but it also
 *  supports eventual default parameters.
 *
 * @See @NamespaceSect{../../,Advanced} section,
 *  @NamespaceRef{../../,BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX},
 *  @NamespaceRef{../../,BOOST_LOCAL_FUNCTION},
 *  Boost.Function.
 */ 
template<typename F, size_t defaults = 0> // Defaults count is opt.
struct function_ref {}; // Empty so never used directly.

}} // namespace boost::local

// Template specializations (must be #included here after the above template).
#include "aux_/function_ref.hpp"

#endif // #include guard

