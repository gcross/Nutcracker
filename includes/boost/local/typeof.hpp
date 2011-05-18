
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Deduce the type of variables that are bound to local functions,
 *  local blocks, and local exits.
 */

#ifndef BOOST_LOCAL_TYPEOF_HPP_
#define BOOST_LOCAL_TYPEOF_HPP_

#include "aux_/symbol.hpp"

// Bound variable name. Expand to qualified bound type (i.e., bound variable
// type with extra const and/or & for const and/or reference binds).
// Can be used with local functions, blocks, and exits. It accepts `this`.
/**
 * @brief This macro expands to the fully qualified type of a variable bound
 *  to to local functions, local blocks, and local exits.
 *
 * The type is fully qualified in that it contains the extra constant and
 * reference qualifiers when they are specified for binding by constant and by
 * reference.
 * For example, if a variable named <c>t</c> of type <c>T</c> is:
 * @li Bound by value using <c>bind t</c> then
 *  <c>BOOST_LOCAL_TYPEOF(t)</c> is <c>T</c>.
 * @li Bound by constant value using <c>const bind t</c>
 *  then <c>BOOST_LOCAL_TYPEOF(t)</c> is <c>const T</c>.
 * @li Bound by reference using <c>bind& t</c>
 *  then <c>BOOST_LOCAL_TYPEOF(t)</c> is <c>T&</c>.
 * @li Bound by constant reference using <c>const bind& t</c>
 *  then <c>BOOST_LOCAL_TYPEOF(t)</c> is <c>const T&</c>.
 *
 * Within local functions, local blocks, and local exits, this macro can be
 * used to deduce the bound variable types to declare local variables, check
 * concepts (using Boost.ConceptCheck), etc (see @RefSect2{Advanced_Topics,
 * Advanced Topics} section).
 *
 * @Params
 * @Param{bound_variable_name,
 *  The name of the bound variable for which the type is being deduced.
 * }
 * @EndParams
 *
 * @See @RefMacro{BOOST_LOCAL_FUNCTION_PARAMS}, @RefMacro{BOOST_LOCAL_BLOCK},
 *  @RefMacro{BOOST_LOCAL_EXIT}, @RefSect2{Advanced_Topics, Advanced Topics}
 *  section.
 */
#define BOOST_LOCAL_TYPEOF(bound_variable_name) \
    BOOST_LOCAL_AUX_SYMBOL_TYPEOF_TYPE(bound_variable_name)

#endif // #include guard

