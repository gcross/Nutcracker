
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_DEDUCE_RESULT_TYPE_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_DEDUCE_RESULT_TYPE_HPP_

#include "../../symbol.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use this lib's ScopeExit impl.
#include <boost/type_traits/remove_pointer.hpp>
#include <boost/type_traits/function_traits.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>

// This must follow the result type.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_DEDUCE_RESULT_TYPE(id, is_template) \
    /* result type here */ (*BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC(id))(); \
    /* the many tagging, wrapping, etc that follow are taken from */ \
    /* Boost.ScopeExit type deduction mechanism and they are necessary */ \
    /* within template on GCC to work around a compiler internal error */ \
    typedef void (*BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_TAG(id))( \
            int BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC(id)); \
    typedef BOOST_PP_IIF(is_template, \
                BOOST_TYPEOF_TPL \
            , \
                BOOST_TYPEOF \
            )(boost::scope_exit::aux::wrap( \
            boost::scope_exit::aux::deref( \
                    BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC(id), \
                    (BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_TAG(id))0))) \
            BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_WRAP(id); \
    typedef BOOST_PP_EXPR_IIF(is_template, typename) \
            BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_WRAP(id)::type \
            BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_CAPTURE(id); \
    struct BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_PARAMS(id) { \
        typedef BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_CAPTURE(id) \
                function_ptr_type;\
    }; \
    typedef BOOST_PP_EXPR_IIF(is_template, typename) boost::remove_pointer< \
            BOOST_PP_EXPR_IIF(is_template, typename) \
            BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_PARAMS(id)::function_ptr_type \
            >::type BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC_TYPE(id); \
    typedef BOOST_PP_EXPR_IIF(is_template, typename) boost::function_traits< \
            BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC_TYPE(id)>::result_type \
            BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id);

#endif // #include guard

