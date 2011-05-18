
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef DOXY // Doxygen documentation only.

#if !BOOST_PP_IS_ITERATING
#   ifndef BOOST_LOCAL_FUNCTION_OVERLOAD_HPP_
#       define BOOST_LOCAL_FUNCTION_OVERLOAD_HPP_

#       include "../config.hpp"
#       include "../aux_/file.hpp"
#       include "../aux_/overload_base.hpp"
#       include <boost/preprocessor/iteration/iterate.hpp>
#       include <boost/preprocessor/repetition/enum.hpp>
#       include <boost/preprocessor/repetition/repeat.hpp>
#       include <boost/preprocessor/control/expr_iif.hpp>
#       include <boost/preprocessor/control/expr_if.hpp>
#       include <boost/preprocessor/comparison/greater.hpp>
#       include <boost/preprocessor/comparison/less.hpp>
#       include <boost/preprocessor/cat.hpp>
#       include <boost/preprocessor/arithmetic/add.hpp>
#       include <boost/preprocessor/tuple/eat.hpp>
#       include <boost/preprocessor/logical/and.hpp>
#       include <boost/preprocessor/logical/not.hpp>
#       include <boost/preprocessor/facilities/expand.hpp>

#define BOOST_LOCAL_f_type(z, f, unused) \
    BOOST_PP_CAT(F, f)

#define BOOST_LOCAL_f_tparam(z, f, unused) \
    typename BOOST_LOCAL_f_type(z, f, unused) \

#define BOOST_LOCAL_f_tparam_dflt(z, f, is_tspec) \
    BOOST_LOCAL_f_tparam(z, f, unused) \
    /* overload requires at least 2 functors so F0 and F1 not optional */ \
    BOOST_PP_EXPR_IIF(BOOST_PP_AND(BOOST_PP_NOT(is_tspec), \
            BOOST_PP_GREATER(f, 1)), \
        = void \
    )

#define BOOST_LOCAL_g_arg_type(z, f, unused) \
    BOOST_PP_CAT(G, f)

#define BOOST_LOCAL_g_arg_name(z, f, unused) \
    BOOST_PP_CAT(g, f)

#define BOOST_LOCAL_g_arg_tparam(z, f, unused) \
    typename BOOST_LOCAL_g_arg_type(z, f, unused)

#define BOOST_LOCAL_g_arg(z, f, unused) \
    /* unfortunately, cannot add const and/or & (not even using */ \
    /* Boost.TypeTraits or Boost.CallTraits) to this function argument */ \
    /* type which needs to remain generic as in its template declaration */ \
    /* (otherwise MSVC cannot deduce the types */ \
    BOOST_LOCAL_g_arg_type(z, f, unused) \
    BOOST_LOCAL_g_arg_name(z, f, unsed)

#define BOOST_LOCAL_overload_base(z, f, unused) \
    ::boost::local::aux::overload_base<BOOST_LOCAL_f_type(z, f, unused)>

#define BOOST_LOCAL_overload_inherit(z, f, unused) \
    public BOOST_LOCAL_overload_base(z, f, unused)

#define BOOST_LOCAL_overload_base_init(z, f, unused) \
    BOOST_LOCAL_overload_base(z, f, unused)( /* base init paren `()` */ \
            BOOST_LOCAL_g_arg_name(z, f, unused))

#define BOOST_LOCAL_using_operator_call(z, f, unused) \
    using BOOST_LOCAL_overload_base(z, f, unused)::operator();

namespace boost { namespace local { namespace function {

// Iterate within namespace.
#       define BOOST_PP_ITERATION_PARAMS_1 \
                /* need at least 2 functors to overload so iter 2, 3, ... */ \
                (3, (0, BOOST_PP_SUB(BOOST_LOCAL_CONFIG_OVERLOAD_MAX, 2), \
                BOOST_LOCAL_AUX_FILE_FUNCTION_OVERLOAD_HPP))
#       include BOOST_PP_ITERATE() // Iterate over function arity.

}}} // namespace boost::local::function

#undef BOOST_LOCAL_f_type
#undef BOOST_LOCAL_f_tparam
#undef BOOST_LOCAL_f_tparam_dflt
#undef BOOST_LOCAL_g_arg_type
#undef BOOST_LOCAL_g_arg_name
#undef BOOST_LOCAL_g_arg_tparam
#undef BOOST_LOCAL_g_arg
#undef BOOST_LOCAL_overload_base
#undef BOOST_LOCAL_overload_inherit
#undef BOOST_LOCAL_overload_base_init
#undef BOOST_LOCAL_using_operator_call

#   endif // #include guard

#elif BOOST_PP_ITERATION_DEPTH() == 1
#   define BOOST_LOCAL_overloads \
        /* iterate as OVERLOADS, OVERLOADS-1, OVERLOADS-2, ... */ \
        BOOST_PP_SUB(BOOST_LOCAL_CONFIG_OVERLOAD_MAX, \
                BOOST_PP_FRAME_ITERATION(1))
#   define BOOST_LOCAL_is_tspec \
        /* if template specialization */ \
        BOOST_PP_LESS(BOOST_LOCAL_overloads, BOOST_LOCAL_CONFIG_OVERLOAD_MAX)

// Iterating within namespace boost::local::function.
template<BOOST_PP_ENUM(BOOST_LOCAL_overloads, BOOST_LOCAL_f_tparam_dflt,
        BOOST_LOCAL_is_tspec)>
class overload
    // Template specialization.
    BOOST_PP_EXPR_IIF(BOOST_PP_EXPAND(BOOST_LOCAL_is_tspec), <)
    BOOST_PP_IIF(BOOST_LOCAL_is_tspec,
        BOOST_PP_ENUM
    ,
        BOOST_PP_TUPLE_EAT(3)
    )(BOOST_LOCAL_overloads, BOOST_LOCAL_f_type, ~)
    BOOST_PP_EXPR_IIF(BOOST_PP_EXPAND(BOOST_LOCAL_is_tspec), >)
    // Bases.
    : // Overloads >= 2 so always at least 2 bases.
    BOOST_PP_ENUM(BOOST_LOCAL_overloads, BOOST_LOCAL_overload_inherit, ~)
{
public:
    template<BOOST_PP_ENUM(BOOST_LOCAL_overloads, BOOST_LOCAL_g_arg_tparam, ~)>
    /* implicit */ inline overload(
            BOOST_PP_ENUM(BOOST_LOCAL_overloads, BOOST_LOCAL_g_arg, ~))
            : // Overloads >= 2 so always at least 2 bases to initialize.
            BOOST_PP_ENUM(BOOST_LOCAL_overloads,
                    BOOST_LOCAL_overload_base_init, ~)
    {}

    BOOST_PP_REPEAT(BOOST_LOCAL_overloads, BOOST_LOCAL_using_operator_call, ~)
};

#   undef BOOST_LOCAL_AUX_overloads
#   undef BOOST_LOCAL_is_tspec
#endif // iteration

#else // DOXY: Doxygen documentation only.

/** @file
 * @brief Defines a functor that can be used to overload the call operator of
 *  a set of specified functors.
 */

namespace boost { namespace local { namespace function {

/**
 * @brief Functor to overload local functions and other functors.
 *
 * This functor aggregates together calls to functions of all the specified
 * function types <c>F0</c>, <c>F1</c>, etc.
 * Each function type must be specified following the Boost.Function preferred
 * syntax:
 * @code
 *  ResultType (ArgumentType0, ArgumgnetType1, ...)
 * @endcode
 *
 * The maximum number of overloaded function types is specified by the
 * @RefMacro{BOOST_LOCAL_CONFIG_OVERLOAD_MAX} configuration macro.
 * The maximum number of function parameters for each of the specified function
 * type is specified by the @RefMacro{BOOST_LOCAL_CONFIG_OVERLOAD_MAX}
 * configuration macro.
 *
 * @See @RefSect2{Advanced_Topics, Advanced Topics} section,
 *  @RefMacro{BOOST_LOCAL_CONFIG_OVERLOAD_MAX},
 *  @RefMacro{BOOST_LOCAL_CONFIG_OVERLOAD_MAX}, Boost.Function.
 */
template<typename F0, typename F1, ...>
class overload {
public:
    /**
     * @brief Construct the overloading functor.
     *
     * Any functor that can be converted to a <c>boost::function</c> funcotr
     * can be specified (local functions, function pointers, other functors,
     * etc).
     */
    overload(const boost::function<F0>&, const boost::function<F1>&, ...);

    /**
     * @brief Call operator matching the signature of the function type
     *  specified as first.
     *
     * This will in turn invoke the call operator of the first functor that was
     * passed to the constructor.
     */
    result_type<F0> operator()(arg0_type<F0>, arg1_type<F0>, ...) const;

    /**
     * @brief Call operator matching the signature of the function type
     *  specified as second.
     *
     * This will in turn invoke the call operator of the second functor that
     * was passed to the constructor.
     */
    result_type<F1> operator()(arg0_type<F1>, arg1_type<F1>, ...) const;
};

}}} // namespace boost::local::function

#endif // DOXY

