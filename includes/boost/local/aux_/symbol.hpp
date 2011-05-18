
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_SYMBOL_HPP_
#define BOOST_LOCAL_AUX_SYMBOL_HPP_

/** @todo Remove any macro define here that is not use anywhere. */

#include "../config.hpp"
#include <boost/preprocessor/cat.hpp>

// INFIX is used to separate symbols concatenamed together. Some of these
// symbols are user defined so they can be anything. Because they can contain
// underscore `_` and/or start with capital letters, it is NOT safe for the
// INFIX to be the underscore `_` character as that could lead to library
// defined symbols containing double underscores `__` which are reserved.
// C++ standard: (1) "each name that contains a double underscore (_ _) or 
// begins with an underscore followed by an uppercase letter is reserved to 
// the implementation" and (2) "each name that begins with an underscore is 
// reserved to the implementation for use as a name in the global namespace".
#define BOOST_LOCAL_AUX_SYMBOL_INFIX X // `X` used as separator.

// Expand to `...aux...<name>` (internal).
#define BOOST_LOCAL_AUX_INTERNAL_SYMBOL(name) \
    BOOST_PP_CAT(BOOST_PP_CAT(boost_local_aux, \
            BOOST_LOCAL_AUX_SYMBOL_INFIX), name)

// Names (functions, variables, etc) //

// Expand to `<name>...aux...` (internal).
#define BOOST_LOCAL_AUX_SYMBOL_TYPEOF_TYPE(name) \
    BOOST_PP_CAT(BOOST_PP_CAT(name, BOOST_LOCAL_AUX_SYMBOL_INFIX), \
            BOOST_LOCAL_AUX_INTERNAL_SYMBOL(typeof_type))

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC(id) \
    /* symbol (not internal) also gives error if missing result type */ \
    BOOST_PP_CAT(ERROR_missing_result_type_before_the_local_function_parameter_macro_id, id)

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_TAG(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(deduce_result_tag, id))

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_WRAP(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(deduce_result_wrap, id))

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_CAPTURE(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(deduce_result_capture, id))

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_PARAMS(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(deduce_result_params, id))

#define BOOST_LOCAL_AUX_SYMBOL_DEDUCE_RESULT_FUNC_TYPE(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(deduce_result_function_type, \
            id))

#define BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(result_type, id))

#define BOOST_LOCAL_AUX_SYMBOL_THIS_PARAM_NAME \
    BOOST_LOCAL_CONFIG_THIS_PARAM_NAME

#define BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(args)

#define BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(params, id))

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(functor, id))

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_OBJECT_NAME(local_function_name) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            functor, BOOST_LOCAL_AUX_SYMBOL_INFIX), local_function_name))

#define BOOST_LOCAL_AUX_SYMBOL_BINDS_VARIABLE_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(binds)

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTION_TYPE \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(function_type)

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_TYPE \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(functor_type)

#define BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(body)

#define BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(block_function, id))

#define BOOST_LOCAL_AUX_SYMBOL_INIT_RECURSION_FUNCTION_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(init_recursion)

#define BOOST_LOCAL_AUX_SYMBOL_INIT_CALL_FUNCTION_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(init_call)

#endif // #include guard

