
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_HPP_

#include "param.hpp"
#include "../../symbol.hpp"
#include "../../preprocessor/sign/params/all_bind_names.hpp"
#include "../../preprocessor/sign/params/has_any_bind.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use Boost.ScopeExit internal impl.
#include <boost/detail/preprocessor/sign/params.hpp>
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/expr_iif.hpp>
#include <boost/preprocessor/cat.hpp>

// Adapted from `BOOST_SCOPE_EXIT_AUX_IMPL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_SEQ_( \
        sign, all_bind_names, has_this, id, typename_keyword) \
    /* binding tags */ \
    BOOST_PP_EXPR_IIF(has_this, \
        BOOST_SCOPE_EXIT_TYPEDEF_TYPEOF_THIS() \
        BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_TYPE_(id); \
    ) \
    BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
            BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_TAG_DECL, \
            id, all_bind_names) \
    BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL, \
            (id, typename_keyword), all_bind_names) \
    /* binding class */ \
    struct BOOST_SCOPE_EXIT_AUX_PARAMS_T(id) { \
        BOOST_PP_EXPR_IIF(has_this, \
            BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_TYPE_(id) \
            BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_NAME_; \
        ) \
        BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_PARAM_DECL, \
                (id, typename_keyword), all_bind_names) \
        BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_MEMBER, \
                id, all_bind_names) \
    } BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id) = { \
        /* initialize the struct with param values to bind */ \
        BOOST_PP_EXPR_IIF(has_this, this) \
        BOOST_PP_COMMA_IF(BOOST_PP_BITAND(has_this, BOOST_PP_NOT( \
                BOOST_DETAIL_PP_NILSEQ_IS_NIL(all_bind_names)))) \
        BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
                BOOST_SCOPE_EXIT_AUX_PARAM_INIT, id, all_bind_names) \
    };

// Assume has some bind param.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_( \
        sign, params, id, typename_keyword) \
    /* has some bind param then all bind names is never empty nil-seq */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_SEQ_(sign, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_NAME_REMOVE( \
                    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_NAMES( \
                    params)), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ALL_BIND_THIS( \
                    params), \
            id, typename_keyword)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITHOUT_TAGS_( \
        sign, params, id, typename_keyword) \
    struct BOOST_SCOPE_EXIT_AUX_PARAMS_T(id) { \
        /* empty struct */ \
    } BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id);

// Public API.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING( \
        sign, id, typename_keyword) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_ANY_BIND( \
            BOOST_DETAIL_PP_SIGN_PARAMS(sign)), \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_ \
    , \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITHOUT_TAGS_ \
    )(sign, BOOST_DETAIL_PP_SIGN_PARAMS(sign), id, typename_keyword) \
    /* this `declared<>` must be present also when no bind param specified */ \
    boost::scope_exit::aux::declared< boost::scope_exit::aux::resolve< \
        /* cannot prefix with `::` as in `sizeof(:: ...` because the name */ \
        /* must refer to the local variable name to allow multiple local */ \
        /* functions (and exits) within the same scope (however this */ \
        /* does not allow for nesting because local variables cannot be */ \
        /* used in nested code blocks) */ \
        sizeof(BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME) \
    >::cmp1<0>::cmp2 > BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME; \
    BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME.value = \
            &BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id); 

#endif // #include guard

