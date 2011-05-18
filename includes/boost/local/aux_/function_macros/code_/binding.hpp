
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_HPP_

#include "bind_this.hpp"
#include "../../symbol.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use this lib's ScopeExit impl.
#include "../../preprocessor/sign/params_any_bind.hpp"
#include "../../preprocessor/sign/param_any_bind.hpp"
#include <boost/type_traits/remove_reference.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/expr_iif.hpp>
#include <boost/preprocessor/list/adt.hpp> // For `IS_CONS`.
#include <boost/preprocessor/list/for_each_i.hpp>
#include <boost/preprocessor/cat.hpp>

// PRIVATE //

// Adapted from `BOOST_SCOPE_EXIT_AUX_TAG_DECL()` (not for `this`).
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_TAG_DECL_TYPED_( \
        r, id, i, typed_var) \
    typedef void (*BOOST_SCOPE_EXIT_AUX_TAG(id, i))(typed_var);

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_TAG_DECL_(r, id, i, bind) \
    BOOST_SCOPE_EXIT_AUX_TAG_DECL(r, id, i, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(bind))

// Adapted from `BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL()` (not for `this`).
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_CAPTURE_DECL_TYPED_( \
        r, id_typename, i, bind) \
    typedef \
        /* remove ref because typed var can have & prefix */ \
        BOOST_PP_TUPLE_ELEM(2, 1, id_typename) /* eventual typename */ \
        ::boost::remove_reference< \
            BOOST_PP_TUPLE_ELEM(2, 1, id_typename) /* eventual typename */ \
            ::boost::function_traits< \
                /* instead of using Boost.Typeof, get bind type from func */ \
                /* type `void (bind_type [&] bind_name)` */ \
                void (BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITH_TYPE(bind)) \
            >::arg1_type \
        >::type \
        BOOST_SCOPE_EXIT_AUX_CAPTURE_T(BOOST_PP_TUPLE_ELEM(2, 0, id_typename), \
                i, BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(bind)) \
    ; /* end typedef */

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_CAPTURE_DECL_( \
        r, id_typename, i, bind) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_HAS_TYPE(bind), \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_CAPTURE_DECL_TYPED_ \
    , \
        BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL \
    )(r, id_typename, i, \
        BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_HAS_TYPE(bind), \
            bind BOOST_PP_TUPLE_EAT(1) \
        , \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE \
        )(bind) \
    )

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_PARAM_DECL_( \
        r, id_typename, i, bind) \
    BOOST_SCOPE_EXIT_AUX_PARAM_DECL(r, id_typename, i, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(bind))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_MEMBER_(r, id, i, bind) \
    BOOST_SCOPE_EXIT_AUX_MEMBER(r, id, i, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(bind))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_PARAM_INIT_(r, id, i, bind) \
    BOOST_SCOPE_EXIT_AUX_PARAM_INIT(r, id, i, \
            BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_WITHOUT_TYPE(bind))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_WITH_( \
        id, all_bind_this_types) \

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_WITHOUT_( \
        id, all_bind_this_types) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id);

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPEDEF_WITH_( \
        all_bind_this_types) \
    typedef BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_THIS_TYPE( \
            BOOST_PP_LIST_FIRST(all_bind_this_types))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPEDEF_WITHOUT_( \
        all_bind_this_types) \
    BOOST_SCOPE_EXIT_TYPEDEF_TYPEOF_THIS()

// Precondition: all_bind_this_type is list with 1 elem (possibly PP_EMPTY).
// Otherwise got a parsing error before getting here.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPE_( \
        id, all_bind_this_types) \
    /* typedef ... */ \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAM_ANY_BIND_THIS_HAS_TYPE( \
            BOOST_PP_LIST_FIRST(all_bind_this_types)), \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPEDEF_WITH_ \
    , \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPEDEF_WITHOUT_ \
    )(all_bind_this_types) \
    /* ... this_type ## id */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
    ; /* end typedef */

// Adapted from `BOOST_SCOPE_EXIT_AUX_IMPL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_SEQ_( \
        all_binds, all_bind_this_types, id, typename_keyword) \
    /* binding tags */ \
    BOOST_PP_IIF(BOOST_PP_LIST_IS_CONS(all_bind_this_types), \
        BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_THIS_TYPE_ \
    , \
        BOOST_PP_TUPLE_EAT(2) \
    )(id, all_bind_this_types) \
    BOOST_PP_LIST_FOR_EACH_I(BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_TAG_DECL_, \
            id, all_binds) \
    BOOST_PP_LIST_FOR_EACH_I( \
            BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_CAPTURE_DECL_, \
            (id, typename_keyword), all_binds) \
    /* binding class */ \
    struct BOOST_SCOPE_EXIT_AUX_PARAMS_T(id) { \
        BOOST_PP_EXPR_IIF(BOOST_PP_LIST_IS_CONS(all_bind_this_types), \
            BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
            BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_NAME; \
        ) \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_PARAM_DECL_, \
                (id, typename_keyword), all_binds) \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_MEMBER_, \
                id, all_binds) \
    } BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id) = { \
        /* initialize the struct with param values to bind */ \
        BOOST_PP_EXPR_IIF(BOOST_PP_LIST_IS_CONS(all_bind_this_types), this) \
        BOOST_PP_COMMA_IF(BOOST_PP_BITAND( \
                BOOST_PP_LIST_IS_CONS(all_bind_this_types), \
                BOOST_PP_LIST_IS_CONS(all_binds))) \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_PARAM_INIT_, \
                id, all_binds) \
    };

// Assume has some bind param.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_( \
        sign_params, id, typename_keyword) \
    /* has some bind param then all bind names is never empty nil-seq */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_WITH_TAGS_SEQ_( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_WITHOUT_THIS(sign_params), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_ALL_BIND_THIS_TYPE(sign_params), \
            id, typename_keyword)

// PUBLIC //

#define BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING( \
        sign_params, id, typename_keyword) \
    /* The binding data structures must be declared and initialized (to */ \
    /* empty structs, so hopefully the compiler will optimize away the */ \
    /* no-op code) even when there is no bound param because these structs */ \
    /* are used to init `...args.value` which is always used by the `NAME` */ \
    /* macro later because this macro does not know if there are bound */ \
    /* params or not */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BINDING_(sign_params, id, typename_keyword) \
    /* this code takes advantage of the template argument list/comparison */ \
    /* operator ambiguity to declare a variable iff it hasn't already been */ \
    /* declared in that scope; the second occurrence is parsed as: */ \
    /*  (boost::scope_exit::aux::declared<(boost::scope_exit::aux::resolve< */ \
    /*  sizeof(boost_local_auxXargs)>::cmp1 < 0)>::cmp2 > ...Xargs); */ \
    /* which is a no-op */ \
    boost::scope_exit::aux::declared< boost::scope_exit::aux::resolve< \
        /* cannot prefix with `::` as in `sizeof(:: ...` because the name */ \
        /* must refer to the local variable name to allow multiple local */ \
        /* functions (and exits) within the same scope (however this */ \
        /* does not allow for nesting because local variables cannot be */ \
        /* used in nested code blocks) */ \
        sizeof(BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME) \
    >::cmp1<0>::cmp2 > BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME; \
    /* stores bound types/values into `...args` variable (args variable */ \
    /* can be accessed by `NAME` macro because doesn't use __LINE__ id) */ \
    BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME.value = \
            &BOOST_LOCAL_AUX_SYMBOL_PARAMS_LOCAL_VARIABLE_NAME(id);

#endif // #include guard

