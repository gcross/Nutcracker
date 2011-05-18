
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_PARAMS_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_PARAMS_HPP_

#include "code_/binding.hpp"
#include "code_/functor.hpp"
#include "code_/deduce_result_type.hpp"
#include "../symbol.hpp"
#include "../scope_exit/scope_exit.hpp" // Use this lib's ScopeExit impl.
#include "../preprocessor/sign/parse_params.hpp"
#include "../preprocessor/sign/params_error.hpp"
#include <boost/mpl/assert.hpp>
#include <boost/preprocessor/control/iif.hpp>

// Undefine local function bound args global variable. Actual declaration of
// this variable is made using SFINAE mechanisms by each local function macro.
extern boost::scope_exit::aux::undeclared
        BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME;

// PRIVATE //

#define BOOST_LOCAL_AUX_FUNCTION_PARAMS_TYPENAME_KEYWORD_(is_template) \
    BOOST_PP_IIF(is_template, BOOST_PP_IDENTITY(typename), BOOST_PP_EMPTY)()

#define BOOST_LOCAL_AUX_FUNCTION_PARAMS_OK_(sign_params, id, is_template) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING(sign_params, id, \
            BOOST_LOCAL_AUX_FUNCTION_PARAMS_TYPENAME_KEYWORD_(is_template)) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR(sign_params, id, \
            BOOST_LOCAL_AUX_FUNCTION_PARAMS_TYPENAME_KEYWORD_(is_template))

#define BOOST_LOCAL_AUX_FUNCTION_PARAMS_ERROR_(sign_params, id, is_template) \
    ; /* close eventual previous statements, otherwise it has no effect */ \
    BOOST_MPL_ASSERT_MSG(false, /* always fails (there's an error) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ERROR_MSG(sign_params), ()) \
    ; /* must close ASSERT macro for eventual use within class scope */

// sign_params: parsed parenthesized params.
#define BOOST_LOCAL_AUX_FUNCTION_PARAMS_(sign_params, id, is_template) \
    /* this must follow the result type (this code must be always present */ \
    /* even if there's an error because result type always present) */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_DEDUCE_RESULT_TYPE(id, is_template) \
    /* rest of the code */ \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_ERROR(sign_params), \
        BOOST_LOCAL_AUX_FUNCTION_PARAMS_ERROR_ \
    , \
        BOOST_LOCAL_AUX_FUNCTION_PARAMS_OK_ \
    )(sign_params, id, is_template)

// PUBLIC //

#define BOOST_LOCAL_AUX_FUNCTION_PARAMS(parenthesized_params, id, is_template) \
    BOOST_LOCAL_AUX_FUNCTION_PARAMS_( \
            BOOST_LOCAL_AUX_PP_SIGN_PARSE_PARAMS(parenthesized_params), \
            id, is_template)

#endif // #include guard

