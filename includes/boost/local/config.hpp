
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

/** @file
 * @brief Configuration macros allow to change the behaviour of this library
 *  at compile-time.
 */

#ifndef BOOST_LOCAL_CONFIG_HPP_
#define BOOST_LOCAL_CONFIG_HPP_

/**
 * @brief Force to use ISO C++ standard features only.
 *
 * If programmers leave this configuration macro undefined, its default
 * value is to be left not defined.
 *
 * If this macro is defined, variadic macros and empty macro parameters are not
 * used by this library. Using variadic macros and empty macro parameters
 * allows this library to provide the <em>variadic macro</em> and <em>empty
 * macro</em> syntaxes which some programmers might find more readable than the
 * <em>sequencing macro</em> syntax (see the @RefSect{Tutorial} section,
 * @RefMacro{BOOST_LOCAL_FUNCTION_PARAMS}, @RefMacro{BOOST_LOCAL_BLOCK}, and
 * @RefMacro{BOOST_LOCAL_EXIT}). If this configuration macro is defined then
 * only the sequencing macro syntax is allowed (regardless of whether the
 * compiler supports variadic and empty macros or not).
 *
 * @Warning The variadic and empty macro syntaxes are not supported by all C++
 *  compilers so they should be used with care to avoid portability issues
 *  (and this configuration macro can be defined to disable them).
 *  Variadic macros, are supported by most recent compilers (like MSVC and
 *  GCC), they were first introduced by the C99 standard, they are part of the
 *  C++0x standard, but they are not part of the official ISO C++ standard.
 *  Empty macro parameters are also supported by the C99 standards and they are
 *  part of the C++0x standard, but they are not supported by all modern
 *  compilers (for example, they are not supported by MSVC).
 *
 * Furthermore, if this macro is defined then the library will not take advantage of
 * compilers that allow to pass local types as template parameters (e.g., MSVC
 * and C++0x compilers) in order to generate code that has more chances to be
 * optimized by the compiler for faster run-times (i.e., inlining the local
 * function calls, see the @RefSect2{Advanced_Topics, Advanced Topics}
 * section).
 *
 * @Note This library internally uses Boost.Typeof to automatically deduce
 *  the bound variable types. The macro symbol <a href='http://www.boost.org/doc/libs/1_46_1/doc/html/typeof/refe.html#typeof.compl'><c>BOOST_TYPEOF_COMPLIANT</c></a>
 *  needs to be defined separately from this configuration macro if programmers
 *  do not want to use non ISO C++ standard support for type deduction
 *  operations.
 * 
 * @See @RefSect{Tutorial} section, @RefSect2{Advanced_Topics, Advanced Topics}
 *  section, @RefSect2{Getting_Started, Getting Started} section.
 */
#ifndef BOOST_LOCAL_CONFIG_COMPLIANT
#undef BOOST_LOCAL_CONFIG_COMPLIANT
#else
// Undefine and define again for doxygen comment above.
#undef BOOST_LOCAL_CONFIG_COMPLIANT
#define BOOST_LOCAL_CONFIG_COMPLIANT
#endif

/**
 * @brief Maximum number of function parameters supported for the local
 *  functions.
 *
 * If programmers leave this configuration macro undefined, its default
 * value is <c>5</c>.
 * 
 * This only applies to the number of local function parameters and not to the
 * number of bound variables in scope (the limit on the number of bound
 * variables is instead the maximum size allowed for a Boost.Preprocessor
 * sequence).
 *
 * @Warning Increasing this number will increase compilation time.
 *
 * @See @RefSect{Tutorial} section, @RefSect2{Getting_Started, Getting Started}
 *  section.
 */
#ifndef BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX
#define BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX 5
#endif

/**
 * @brief The number of function types that can be overloaded by the
 *  @RefClass{boost::local::function::overload} functor.
 *
 * If programmers leave this configuration macro undefined, its default
 * value is <c>6</c>.
 *
 * This number must be greater or equal than 2 (because at least two function
 * types are needed to define an overloaded call operator) otherwise the
 * library will generate a compile-time error.
 *
 * @Warning Increasing this number will increase compilation time when the
 *  header file <c>"boost/local/function/overload.hpp"</c> is included.
 *
 * @See @RefSect2{Advanced_Topics, Advanced Topics} section,
 *  @RefSect2{Getting_Started, Getting Started} section.
 */
#ifndef BOOST_LOCAL_CONFIG_OVERLOAD_MAX
#define BOOST_LOCAL_CONFIG_OVERLOAD_MAX 6
#endif
#if BOOST_LOCAL_CONFIG_OVERLOAD_MAX < 2
#error "BOOST_LOCAL_CONFIG_OVERLOAD_MAX must be grater or equal than 2"
#endif

/**
 * @brief The name of the special symbol used to access the bound object
 * <c>this</c> within the body of the local constructs.
 *
 * If programmers leave this configuration macro undefined, the default
 * symbol used is <c>this_</c> (this default symbol follows Boost's conventions
 * to postfix with an underscore symbols that refer to C++ keywords -- the
 * <c>this</c> keyword in this case).
 *
 * @Warning Programmers should not define this macro unless it is absolutely
 *  necessary (e.g., to avoid name clashes with another library which cannot be
 *  changed). Changing the symbol <c>this_</c> effectively changes the public
 *  API of this library.
 *
 * @See @RefSect{Tutorial} section, @RefSect2{Getting_Started, Getting Started}
 *  section.
 */
#ifndef BOOST_LOCAL_CONFIG_THIS_PARAM_NAME
#define BOOST_LOCAL_CONFIG_THIS_PARAM_NAME this_
#endif

#endif // #include guard

