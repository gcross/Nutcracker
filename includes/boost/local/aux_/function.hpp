
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#if !BOOST_PP_IS_ITERATING
#   ifndef BOOST_LOCAL_AUX_FUNCTION_HPP_
#       define BOOST_LOCAL_AUX_FUNCTION_HPP_

#       include "file.hpp"
#       include "../config.hpp"
#       include <boost/call_traits.hpp>
#       include <boost/preprocessor/iteration/iterate.hpp>
#       include <boost/preprocessor/repetition/repeat.hpp>
#       include <boost/preprocessor/repetition/enum.hpp>
#       include <boost/preprocessor/punctuation/comma_if.hpp>
#       include <boost/preprocessor/arithmetic/sub.hpp>
#       include <boost/preprocessor/arithmetic/inc.hpp>
#       include <boost/preprocessor/cat.hpp>

#define BOOST_LOCAL_AUX_arg_type(z, arg_n, unused) \
    BOOST_PP_CAT(A, arg_n)

#define BOOST_LOCAL_AUX_arg_param_type(z, arg_n, unused) \
    typename ::boost::call_traits<BOOST_LOCAL_AUX_arg_type(z, arg_n, unused) \
            >::param_type

#define BOOST_LOCAL_AUX_arg_name(z, arg_n, unused) \
    BOOST_PP_CAT(a, arg_n)

#define BOOST_LOCAL_AUX_arg(z, arg_n, unused) \
    BOOST_LOCAL_AUX_arg_param_type(z, arg_n, unused) \
    BOOST_LOCAL_AUX_arg_name(z, arg_n, unused)

#define BOOST_LOCAL_AUX_arg_tparam(z, arg_n, unused) \
    typename BOOST_LOCAL_AUX_arg_type(z, arg_n, unused)

#define BOOST_LOCAL_AUX_call_ptr(z, n, unused) \
    BOOST_PP_CAT(call_ptr, n)

#define BOOST_LOCAL_AUX_call_name(z, n, unused) \
    BOOST_PP_CAT(call, n)

#define BOOST_LOCAL_AUX_call_member(z, n, unused) \
    BOOST_PP_CAT(BOOST_LOCAL_AUX_call_name(z, n, unused), _)

#define BOOST_LOCAL_AUX_call_typedef(z, n, arity) \
    typedef R (*BOOST_LOCAL_AUX_call_ptr(z, n, ~))(object_ptr \
            BOOST_PP_COMMA_IF(BOOST_PP_SUB(arity, n)) \
            BOOST_PP_ENUM_ ## z(BOOST_PP_SUB(arity, n), \
                    BOOST_LOCAL_AUX_arg_param_type, ~));

#define BOOST_LOCAL_AUX_call_param(z, n, unused) \
    BOOST_LOCAL_AUX_call_ptr(z, n, unused) \
    BOOST_LOCAL_AUX_call_name(z, n, unused)

#define BOOST_LOCAL_AUX_call_decl(z, n, unused) \
    BOOST_LOCAL_AUX_call_ptr(z, n, unused) \
    BOOST_LOCAL_AUX_call_member(z, n, unused);

#define BOOST_LOCAL_AUX_call_init(z, n, unused) \
    BOOST_LOCAL_AUX_call_member(z, n, unused) = \
            BOOST_LOCAL_AUX_call_name(z, n, unuzed);
                
#define BOOST_LOCAL_AUX_operator_call(z, defaults_n, arity) \
    /* precondition: object_ && call_function_ */ \
    inline R operator()(BOOST_PP_ENUM_ ## z(BOOST_PP_SUB(arity, defaults_n), \
                BOOST_LOCAL_AUX_arg, ~)) const { \
        /* run-time: do not assert preconditions here for efficiency */ \
        /* run-time: this function call is done via a function pointer */ \
        /* so unfortunately does not allow for compiler inlining */ \
        /* optimizations (an alternative using virtual function was also */ \
        /* investigated but also virtual functions cannot be optimized */ \
        /* plus they require virtual table lookups to the alternative */ \
        /* performed worst) */ \
        return BOOST_LOCAL_AUX_call_member(z, defaults_n, ~)(object_ \
                BOOST_PP_COMMA_IF(BOOST_PP_SUB(arity, defaults_n)) \
                BOOST_PP_ENUM_ ## z(BOOST_PP_SUB(arity, defaults_n), \
                        BOOST_LOCAL_AUX_arg_name, ~)); \
    }

namespace boost { namespace local { namespace aux {

template<typename F, size_t defaults = 0>
class function {
    // Empty template cannot be used directly (only via its specializations).
};

// Iterate within namespace.
#       define BOOST_PP_ITERATION_PARAMS_1 \
                (3, (0, BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX, \
                BOOST_LOCAL_AUX_FILE_FUNCTION_HPP))
#       include BOOST_PP_ITERATE() // Iterate over function arity.

}}} // namespace boost::loca::aux

#undef BOOST_LOCAL_AUX_arg_type
#undef BOOST_LOCAL_AUX_arg_param_type
#undef BOOST_LOCAL_AUX_arg_name
#undef BOOST_LOCAL_AUX_arg
#undef BOOST_LOCAL_AUX_arg_tparam
#undef BOOST_LOCAL_AUX_call_ptr
#undef BOOST_LOCAL_AUX_call_name
#undef BOOST_LOCAL_AUX_call_member
#undef BOOST_LOCAL_AUX_call_typedef
#undef BOOST_LOCAL_AUX_call_param
#undef BOOST_LOCAL_AUX_call_decl
#undef BOOST_LOCAL_AUX_call_init
#undef BOOST_LOCAL_AUX_operator_call

#   endif // #include guard

#elif BOOST_PP_ITERATION_DEPTH() == 1
#   define BOOST_LOCAL_AUX_arity BOOST_PP_FRAME_ITERATION(1)
#   define BOOST_PP_ITERATION_PARAMS_2 \
            (3, (0, BOOST_LOCAL_AUX_arity, \
            BOOST_LOCAL_AUX_FILE_FUNCTION_HPP))
#   include BOOST_PP_ITERATE() // Iterate over default params count.
#   undef BOOST_LOCAL_AUX_arity

#elif BOOST_PP_ITERATION_DEPTH() == 2
#   define BOOST_LOCAL_AUX_defaults BOOST_PP_FRAME_ITERATION(2)

// Iterating within namespce boost::local::aux.
template<typename R
    BOOST_PP_COMMA_IF(BOOST_LOCAL_AUX_arity)
    BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity, BOOST_LOCAL_AUX_arg_tparam, ~)
>
class function<
      R (BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity, BOOST_LOCAL_AUX_arg_type, ~))
    , BOOST_LOCAL_AUX_defaults
> {
    // The object type will actually be a local class which cannot be passed as
    // a template parameter so a generic `void*` pointer is used to hold the
    // object (this pointer will then be cased by the call-function implemented
    // by the local class itself). This is the trick used to pass a local
    // function as a template parameter. This trick uses function pointers for
    // the call-functions and function pointers cannot always be optimized by
    // the compiler (they cannot be inlined) thus this trick increased run-time
    // (another trick using virtual functions for the local class was also
    // investigated but also virtual functions cannot be inlined plus they
    // require virtual tables lookups so the virtual functions trick measured
    // worst run-time performance than the function pointer trick).
    typedef void* object_ptr;
    BOOST_PP_REPEAT(BOOST_PP_INC(BOOST_LOCAL_AUX_defaults), // INC for no dflt.
            BOOST_LOCAL_AUX_call_typedef, BOOST_LOCAL_AUX_arity)
public:
    // run-time: use compiler-generated default constructor, copy constructor,
    // and copy operator (this class only has pointers as member variables and
    // they only need to be copied shallowly so the compiler-generator
    // operations work well) to allow for compiler optimization

    // Cannot be private but it should never be used by programmers directly
    // so used internal symbol.
    inline void BOOST_LOCAL_AUX_SYMBOL_INIT_CALL_FUNCTION_NAME(
        object_ptr object,
        BOOST_PP_ENUM(BOOST_PP_INC(BOOST_LOCAL_AUX_defaults), // INC no dflt.
                BOOST_LOCAL_AUX_call_param, ~)
    ) {
        object_ = object;
        BOOST_PP_REPEAT(BOOST_PP_INC(BOOST_LOCAL_AUX_defaults),
                BOOST_LOCAL_AUX_call_init, ~)
        unused_ = 0; // To avoid a GCC uninitialized variable error.
    }
    
    // Result operator(Arg1, ..., ArgN-1, ArgN) -- iff defaults >= 0
    // Result operator(Arg1, ..., ArgN-1)       -- iff defaults >= 1
    // ...                                      -- etc
    BOOST_PP_REPEAT(BOOST_PP_INC(BOOST_LOCAL_AUX_defaults), // INC for no dflt.
            BOOST_LOCAL_AUX_operator_call, BOOST_LOCAL_AUX_arity)

private:
    object_ptr object_;
    BOOST_PP_REPEAT(BOOST_PP_INC(BOOST_LOCAL_AUX_defaults), // INC for no dflt.
            BOOST_LOCAL_AUX_call_decl, ~)

    // run-time: this unused void* member variable allows for compiler
    // optimizations (at least on MSVC it reduces invocation time of about 50%)
    void* unused_;
};

#   undef BOOST_LOCAL_AUX_defaults
#endif // iteration

