
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#if !BOOST_PP_IS_ITERATING
#   ifndef BOOST_LOCAL_AUX_FUNCTION_BASE_HPP_
#       define BOOST_LOCAL_AUX_FUNCTION_BASE_HPP_

#       include "../file.hpp"
#       include "../../config.hpp"
#       include <boost/preprocessor/iteration/iterate.hpp>
#       include <boost/preprocessor/repetition/repeat.hpp>
#       include <boost/preprocessor/repetition/enum.hpp>
#       include <boost/preprocessor/punctuation/comma_if.hpp>
#       include <boost/preprocessor/arithmetic/sub.hpp>
#       include <boost/preprocessor/arithmetic/inc.hpp>
#       include <boost/preprocessor/cat.hpp>

#define BOOST_LOCAL_arg_type(z, arg_n, unused) \
    BOOST_PP_CAT(A, arg_n)

#define BOOST_LOCAL_arg_typename(z, arg_n, unused) \
    typename BOOST_LOCAL_arg_type(z, arg_n, unused)
                
#define BOOST_LOCAL_operator_call(z, defaults_n, arity) \
    virtual R operator()(BOOST_PP_ENUM_ ## z(BOOST_PP_SUB(arity, defaults_n), \
                BOOST_LOCAL_arg_type, ~)) = 0;

#define BOOST_LOCAL_template_name(defaults_count) \
    BOOST_PP_CAT(BOOST_PP_CAT(functor_defaults, defaults_count), _base) 

namespace boost { namespace local { namespace aux {

template<typename F, unsigned int defaults_count = 0> // Defaults count is opt.
struct function_base {}; // Empty so never used directly.

// Iteration within the namespace.
#       define BOOST_PP_ITERATION_PARAMS_1 \
                (3, (0, BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX, \
                BOOST_LOCAL_AUX_FILE_FUNCTION_BASE_HPP))
#       include BOOST_PP_ITERATE() // Iterate over arity.

}}} // namespace boost::local::aux

#   undef BOOST_LOCAL_arg_type
#   undef BOOST_LOCAL_arg_typename
#   undef BOOST_LOCAL_operator_call
#   undef BOOST_LOCAL_template_name

#   endif // #include guard

#elif BOOST_PP_ITERATION_DEPTH() == 1
#   define BOOST_LOCAL_arity BOOST_PP_FRAME_ITERATION(1)

#   define BOOST_PP_ITERATION_PARAMS_2 \
            (3, (0, BOOST_LOCAL_arity, \
            BOOST_LOCAL_AUX_FILE_FUNCTION_BASE_HPP))
#   include BOOST_PP_ITERATE() // Iterate over default params count.
#   undef BOOST_LOCAL_arity

#elif BOOST_PP_ITERATION_DEPTH() == 2
#   define BOOST_LOCAL_defaults_count BOOST_PP_FRAME_ITERATION(2)

template<typename R
        BOOST_PP_COMMA_IF(BOOST_LOCAL_arity)
        BOOST_PP_ENUM(BOOST_LOCAL_arity,
                BOOST_LOCAL_arg_typename, ~)>
struct function_base<
        R (BOOST_PP_ENUM(BOOST_LOCAL_arity, 
                BOOST_LOCAL_arg_type, ~)),
        BOOST_LOCAL_defaults_count> {
    /* PP_INC to also handle no defaults */ \
    BOOST_PP_REPEAT(BOOST_PP_INC(BOOST_LOCAL_defaults_count),
            BOOST_LOCAL_operator_call, BOOST_LOCAL_arity)
};

#   undef BOOST_LOCAL_defaults_count

#endif // iteration

