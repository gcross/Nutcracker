
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_SYMBOL_HPP_
#define BOOST_LOCAL_AUX_SYMBOL_HPP_

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

#define BOOST_LOCAL_AUX_SYMBOL_PREFIX \

// Expand to `contract...<name>`.
#define BOOST_LOCAL_AUX_SYMBOL(name) \
    BOOST_PP_CAT(BOOST_PP_CAT(boost_local, \
            BOOST_LOCAL_AUX_SYMBOL_INFIX), name)

// Expand to `contract...aux...<name>` (internal).
#define BOOST_LOCAL_AUX_INTERNAL_SYMBOL(name) \
    BOOST_PP_CAT(BOOST_PP_CAT(boost_local_aux, \
            BOOST_LOCAL_AUX_SYMBOL_INFIX), name)

// Names (functions, variables, etc) //

#define BOOST_LOCAL_AUX_SYMBOL_THIS_PARAM_NAME \
    BOOST_LOCAL_CONFIG_THIS_PARAM_NAME

#define BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(args)

#define BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(params_, id))

#define BOOST_LOCAL_AUX_SYMBOL_PARAMS_VARIABLE_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(params)

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME( \
        local_function_name) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            functor, BOOST_LOCAL_AUX_SYMBOL_INFIX), \
            local_function_name))

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_OBJECT_NAME( \
        local_function_name) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            object, BOOST_LOCAL_AUX_SYMBOL_INFIX), \
            local_function_name))

#define BOOST_LOCAL_AUX_SYMBOL_FUNCTION_REF_NAME( \
        local_function_name) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            local_function_name, BOOST_LOCAL_AUX_SYMBOL_INFIX), ref))

#define BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(body)

#define BOOST_LOCAL_AUX_SYMBOL_BLOCK_FUNCTION_NAME \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(block_function)

#endif // #include guard

