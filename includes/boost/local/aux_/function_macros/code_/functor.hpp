
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HPP_

#include "bind_this.hpp"
#include "../../symbol.hpp"
#include "../../preprocessor/sign/params_unbind.hpp"
#include "../../preprocessor/sign/params_const_bind.hpp"
#include "../../preprocessor/sign/params_bind.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use this lib's ScopeExit impl.
#include "../../type_traits/add_pointed_const.hpp"
#include "../../function.hpp"
#include "../../../config.hpp"
#include <boost/detail/preprocessor/keyword/auto.hpp>
#include <boost/detail/preprocessor/keyword/register.hpp>
#include <boost/type_traits/add_const.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/logical/bitand.hpp>
#include <boost/preprocessor/logical/bitand.hpp>
#include <boost/preprocessor/logical/bitor.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/expr_iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/list/adt.hpp> // For `IS_CONS`.
#include <boost/preprocessor/list/for_each_i.hpp>

// PRIVATE //

// Unbind parameters.

// i: 1 for 1st param, 2 for 2nd, ... (start from 1 not 0).
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_(i) \
    /* this must be a generic parameter name because unbind type and name */ \
    /* are not separate tokens in the macro syntax so name is not available */ \
    /* separately from its type */ \
    BOOST_PP_CAT(arg, i)

// i: 1 for 1st param, 2 for 2nd, ... (start from 1 not 0).
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_TYPE_( \
        typename_keyword, i) \
    /* the parameter type must be accessed using function traits from */ \
    /* function type because it is not available to the macro syntax */ \
    /* separately from the parameter name */ \
    typename_keyword \
    BOOST_PP_CAT(BOOST_PP_CAT(::boost::function_traits< \
            BOOST_LOCAL_AUX_SYMBOL_FUNCTION_TYPE>::arg, i), _type) \

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_DECL_( \
        r, typename_keyword, i, param) \
    BOOST_PP_COMMA_IF(i) \
    typename_keyword \
    ::boost::call_traits< \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_TYPE_( \
                    typename_keyword, BOOST_PP_INC(i))>::param_type \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_(BOOST_PP_INC(i))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_ENUM_( \
        r, unused, i, param) \
    BOOST_PP_COMMA_IF(i) /* enumeration commas */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_(BOOST_PP_INC(i))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_DECL_( \
        r, unused, i, param) \
    BOOST_PP_COMMA_IF(i) \
    BOOST_DETAIL_PP_KEYWORD_AUTO_REMOVE_BACK( \
        BOOST_DETAIL_PP_KEYWORD_REGISTER_REMOVE_BACK( \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_PARAM_DECL(param) \
        ) \
    )

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_DECL_WITH_DEFAULT_( \
        r, unused, i, param) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_DECL_(z, unused, i, param) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_PARAM_HAS_DEFAULT( \
            param), \
        = BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_PARAM_DEFAULT \
    , \
        BOOST_PP_TUPLE_EAT(1) \
    )(param)

// Bound parameters.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_PARAM_ \
    bindings /* constructor parameter `void*` bindings pointer */

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_VAR_(i) \
    /* named `bind0`, `bind1`, ... */ \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(bind, i))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_THIS_ \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(bind_this)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_TYPE_( \
        id_typename_offset, i, var) \
    BOOST_SCOPE_EXIT_AUX_PARAMS_T( \
            BOOST_PP_TUPLE_ELEM(3, 0, id_typename_offset)):: \
    BOOST_SCOPE_EXIT_AUX_PARAM_T( \
            BOOST_PP_TUPLE_ELEM(3, 0, id_typename_offset), \
            BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(3, 2, id_typename_offset)),\
            var)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_MEMBER_BIND_( \
        r, offset, i, var) \
    BOOST_PP_COMMA_IF(i) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_VAR_( \
            BOOST_PP_ADD(offset, i))

// Adapted from `BOOST_SCOPE_EXIT_AUX_ARG_DECL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_( \
        r, id_typename_offset, i, var, is_const) \
    BOOST_PP_TUPLE_ELEM(3, 1, id_typename_offset) /* eventual typename */ \
    BOOST_PP_EXPR_IF(is_const, \
        BOOST_PP_IIF( \
                BOOST_DETAIL_PP_KEYWORD_IS_THIS_BACK(var), \
            ::boost::local::aux::add_pointed_const< /* pointed obj const */ \
        , \
            ::boost::add_const< /* outer type const */ \
        ) \
        BOOST_PP_TUPLE_ELEM(3, 1, id_typename_offset) /* eventual typename */ \
    ) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_TYPE_( \
            id_typename_offset, i, var) \
    BOOST_PP_EXPR_IF(is_const, >::type) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_RENAME(var)

// Adapted from `BOOST_SCOPE_EXIT_AUX_ARG_DECL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_ENUM_( \
        r, id_typename_offset, i, var, is_const) \
    BOOST_PP_COMMA_IF(i) /* enumeration commas */ \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_( \
            r, id_typename_offset, i, var, is_const)
    
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_DECL_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_ENUM_( \
            r, id_typename_offset, i, var, 0 /* do not force const */)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_BIND_DECL_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_ENUM_( \
            r, id_typename_offset, i, var, 1 /* force const */)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_DECL_( \
        r, id_typename_offset, i, var, is_const) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_( \
            r, id_typename_offset, i, \
            & /* all bind member vars are refs to ScopeExit struct members */ \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_VAR_(\
                    BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(3, 2, \
                            id_typename_offset))), \
            is_const) \
    ; /* end member variable declaration */

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_DECL_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_DECL_( \
            r, id_typename_offset, i, var, 0 /* do not force const */) \

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_BIND_MEMBER_DECL_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_DECL_( \
            r, id_typename_offset, i, var, 1 /* force const */) \

// Adapted from `BOOST_SCOPE_EXIT_AUX_ARG()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_INIT_( \
        r, id_offset, i, var) \
    BOOST_PP_COMMA_IF(i) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_VAR_( \
            BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(2, 1, id_offset))) \
    ( /* member variable initialization */ \
        static_cast< BOOST_SCOPE_EXIT_AUX_PARAMS_T( \
                BOOST_PP_TUPLE_ELEM(2, 0, id_offset))* >( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_PARAM_)-> \
        BOOST_SCOPE_EXIT_AUX_PARAM( \
                BOOST_PP_TUPLE_ELEM(2, 0, id_offset), \
                BOOST_PP_ADD(i, BOOST_PP_TUPLE_ELEM(2, 1, id_offset)), \
                var).value \
    )

// Typeof type-definitions.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_TYPEDEF_( \
        r, id_typename_offset, i, var, is_const) \
    typedef \
    /* the type with the special typeof name */ \
    BOOST_LOCAL_AUX_SYMBOL_TYPEOF_TYPE( \
        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_DECL_( \
                r, id_typename_offset, i, var, is_const) \
    ) ; /* end typedef */

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_TYPEDEF_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_TYPEDEF_( \
            r, id_typename_offset, i, var, 0 /* do not add const */)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_TYPEDEF_( \
        r, id_typename_offset, i, var) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_TYPEDEF_( \
            r, id_typename_offset, i, var, 1 /* add const */)

// Template parameter function type `F`.

// Expand to the function type `R (A1, ...)`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_F_( \
        sign_params, id, has_type, type_name) \
    BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id) \
    BOOST_PP_EXPR_IIF(has_type, (type_name) ) \
    ( \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_DECL_, ~, \
                BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(sign_params)) \
    )

// Functor call operations.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_(z, \
        defaults_n, \
        sign_params, \
        unbinds, \
        const_binds, has_const_bind_this, \
        binds, has_bind_this, \
        id, typename_keyword) \
    inline BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id) operator()( \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_DECL_, \
                typename_keyword, unbinds) \
    ) const { \
        /* just forward call to member function with local func name */ \
        return BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME( \
            BOOST_PP_LIST_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_MEMBER_BIND_, \
                    0 /* no offset */, const_binds) \
            /* pass plain binds */ \
            BOOST_PP_COMMA_IF( \
                BOOST_PP_BITAND( \
                      BOOST_PP_LIST_IS_CONS(const_binds) \
                    , BOOST_PP_LIST_IS_CONS(binds) \
                ) \
            ) \
            BOOST_PP_LIST_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_MEMBER_BIND_, \
                    /* offset param index of # of preceeding */ \
                    /* const-bind params (could be 0)*/ \
                    BOOST_PP_LIST_SIZE(const_binds), binds) \
            /* pass bind `this` */ \
            BOOST_PP_COMMA_IF( \
                BOOST_PP_BITAND( \
                      BOOST_PP_BITOR( \
                          BOOST_PP_LIST_IS_CONS(const_binds) \
                        , BOOST_PP_LIST_IS_CONS(binds) \
                      ) \
                    , BOOST_PP_BITOR(has_const_bind_this, has_bind_this) \
                ) \
            ) \
            BOOST_PP_EXPR_IIF( \
                    BOOST_PP_BITOR(has_const_bind_this, has_bind_this), \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_THIS_ \
            ) \
            /* pass unbind params */ \
            BOOST_PP_COMMA_IF( \
                BOOST_PP_BITAND( \
                      BOOST_PP_BITOR( \
                          BOOST_PP_BITOR( \
                              BOOST_PP_LIST_IS_CONS(const_binds) \
                            , BOOST_PP_LIST_IS_CONS(binds) \
                          ) \
                        , BOOST_PP_BITOR(has_const_bind_this, has_bind_this) \
                      ) \
                    , BOOST_PP_LIST_IS_CONS(unbinds) \
                ) \
            ) \
            BOOST_PP_LIST_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_ENUM_, \
                    ~, unbinds) \
        ); \
    }

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_FUNC_( \
        z, defaults_n, unused) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(call, defaults_n))

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_FUNC_PTR_( \
        z, defaults_n, unused) \
    &BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_FUNC_( \
            z, defaults_n, unused)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_(z, \
        defaults_n, \
        sign_params, \
        unbinds, \
        const_binds, has_const_bind_this, \
        binds, has_bind_this, \
        id, typename_keyword) \
    inline static BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_FUNC_(z, defaults_n, ~)( \
        void* object \
        BOOST_PP_COMMA_IF(BOOST_PP_LIST_IS_CONS(unbinds)) \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_DECL_, \
                typename_keyword, unbinds) \
    ) { \
        /* run-time: casting object to this class type and forward call to */ \
        /* `operator()` (this performs better than doing multiple casting */ \
        /* or using a casted object local variable here to call body */ \
        /* directly from here without passing via `operator()`) */ \
        /* compliance: passing local class type to `static_cast` is fully */ \
        /* ISO C++ compliant because `static_cast` is not a template (even */ \
        /* if its syntax resembles a function template call) in fact even */ \
        /* in C is legal to cast to a local struct (using C-style casting) */ \
        return static_cast< BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME(id)* >( \
                object)->operator()( \
            BOOST_PP_LIST_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_ARG_NAME_ENUM_, \
                    ~, unbinds) \
        ); \
    }

// Return unbind params but without last (default) params specified by count.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_REMOVE_LAST_N_(n, \
        unbinds) \
    BOOST_PP_LIST_FIRST_N(BOOST_PP_SUB(BOOST_PP_LIST_SIZE(unbinds), n), \
            unbinds)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_FOR_DEFAULTS_(z, n, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    BOOST_PP_EXPAND( \
    BOOST_PP_TUPLE_ELEM(9, 0, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    ( z, n \
    , BOOST_PP_TUPLE_ELEM(9, 1, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    /* remove last n default params */ \
    , BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_REMOVE_LAST_N_(n, \
        BOOST_PP_TUPLE_ELEM(9, 2, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
      ) \
    , BOOST_PP_TUPLE_ELEM(9, 3, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    , BOOST_PP_TUPLE_ELEM(9, 4, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    , BOOST_PP_TUPLE_ELEM(9, 5, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    , BOOST_PP_TUPLE_ELEM(9, 6, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    , BOOST_PP_TUPLE_ELEM(9, 7, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    , BOOST_PP_TUPLE_ELEM(9, 8, \
        op_params_unbinds_constbinds_hasconstthis_binds_hasthis_id_typename) \
    ) /* end `op_macro(...)` */ \
    ) /* end expand */

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MEMBER_INITS_( \
        const_binds, has_const_bind_this, \
        binds, has_bind_this, \
        id) \
    BOOST_PP_EXPR_IIF(BOOST_PP_BITOR(BOOST_PP_BITOR(BOOST_PP_BITOR( \
            BOOST_PP_LIST_IS_CONS(const_binds), BOOST_PP_LIST_IS_CONS(binds)), \
            has_bind_this), has_const_bind_this), \
        : \
    ) \
    /* init const binds */ \
    BOOST_PP_LIST_FOR_EACH_I( \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_INIT_,\
            (id, 0 /* no offset */), \
            const_binds) \
    /* init plain binds */ \
    BOOST_PP_COMMA_IF( \
        BOOST_PP_BITAND( \
              BOOST_PP_LIST_IS_CONS(const_binds) \
            , BOOST_PP_LIST_IS_CONS(binds) \
        ) \
    ) \
    BOOST_PP_LIST_FOR_EACH_I( \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MAYBECONST_BIND_MEMBER_INIT_,\
            (id \
            /* offset param index of # of preceeding */ \
            /* const-bind params (could be 0)*/ \
            , BOOST_PP_LIST_SIZE(const_binds) \
            ), \
            binds) \
    /* init `this` bind (const or not) */ \
    BOOST_PP_COMMA_IF( \
        BOOST_PP_BITAND( \
              BOOST_PP_BITOR( \
                  BOOST_PP_LIST_IS_CONS(const_binds) \
                , BOOST_PP_LIST_IS_CONS(binds) \
              ) \
            , BOOST_PP_BITOR(has_const_bind_this, has_bind_this) \
        ) \
    ) \
    BOOST_PP_EXPR_IIF(BOOST_PP_BITOR(has_const_bind_this, has_bind_this), \
        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_THIS_( \
            static_cast< BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)* >( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_PARAM_)-> \
            BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_NAME \
        ) \
    )

// Functor class.

// Adapted from `BOOST_SCOPE_EXIT_AUX_IMPL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_(sign_params, \
        unbinds, default_count, \
        const_binds, has_const_bind_this, \
        binds, has_bind_this, \
        id, typename_keyword) \
    class BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME(id) \
    /* run-time: do not use base class to allow for compiler optimizations */ \
    { \
        /* function type */ \
        typedef BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_F_(sign_params, id, \
                1 /* has type */, BOOST_LOCAL_AUX_SYMBOL_FUNCTION_TYPE); \
        /* functor type -- this type cannot have ID postfix because it is */ \
        /* used the `NAME` macro (this symbol is within functor class so */ \
        /* it does not have to have ID postfix) */ \
        typedef ::boost::local::aux::function< \
                BOOST_LOCAL_AUX_SYMBOL_FUNCTION_TYPE, \
                default_count> BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_TYPE; \
        /* typeof types -- these types are qualified with extra eventual */ \
        /* const and/or & if their variables are bound by const and/or & */ \
        /* (this is because it is not possible to strip the eventual & */ \
        /* given that the var name is always attached to the & symbol plus */ \
        /* programmers can always remove const& using type traits) */ \
        /* const bind typeof types */ \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_TYPEDEF_, \
                (id, typename_keyword, 0 /* no offset */), \
                const_binds) \
        /* bind typeof types */ \
        BOOST_PP_LIST_FOR_EACH_I( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_TYPEDEF_, \
                ( id, typename_keyword, \
                  /* offset param index of # of preceeding */ \
                  /* const-bindparams (could be 0)*/ \
                  BOOST_PP_LIST_SIZE(const_binds)),\
                binds) \
        /* this (const or not) bind typeof type */ \
        BOOST_PP_EXPR_IIF(has_const_bind_this, \
            typedef \
            BOOST_LOCAL_AUX_SYMBOL_TYPEOF_TYPE( \
                typename_keyword ::boost::local::aux::add_pointed_const< \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
                >::type this /* must not use `this_` for TYPEOF_TYPE */ \
            ) ; /* end typedef */ \
        ) \
        BOOST_PP_EXPR_IIF(has_bind_this, \
            typedef \
            BOOST_LOCAL_AUX_SYMBOL_TYPEOF_TYPE( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
                this /* must not use `this_` for TYPEOF_TYPE */ \
            ) ; /* end typedef */ \
        ) \
    public: \
        /* constructor */ \
        inline explicit BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME(id)( \
                void* BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_PARAM_) \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_MEMBER_INITS_(const_binds, \
                    has_const_bind_this, binds, has_bind_this, id) \
        { /* do nothing */ } \
        /* run-time: implement `operator()` (and for all default params) so */ \
        /* this obj can be used directly as a functor for C++03 extensions */ \
        /* and optimized macros */ \
        BOOST_PP_REPEAT( \
            /* PP_INC to handle no dflt (EXPAND for MVSC) */ \
            BOOST_PP_EXPAND(BOOST_PP_INC(default_count)), \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_FOR_DEFAULTS_,\
            ( BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_, sign_params \
            , unbinds, const_binds, has_const_bind_this, binds \
            , has_bind_this, id, typename_keyword ) \
        ) \
        /* compliance: trick to pass this local class as a template param */ \
        /* on ISO C++ without non C++03 extension */ \
        /* performance: this trick introduced _one_ indirect function call */ \
        /* via a function pointer that cannot be inlined by the complier */ \
        /* thus increasing run-time (also another trick using a base */ \
        /* interface class was investigated but virtual calls also cannot */ \
        /* inlined plus they require virtual table lookups to the "virtual */ \
        /* call trick" measured longer run-times than this "static call */ \
        /* trick") */ \
        BOOST_PP_REPEAT( \
            /* PP_INC to handle no dflt (EXPAND for MVSC) */ \
            BOOST_PP_EXPAND(BOOST_PP_INC(default_count)), \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_FOR_DEFAULTS_,\
            ( BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_, sign_params \
            , unbinds, const_binds, has_const_bind_this, binds \
            , has_bind_this, id, typename_keyword ) \
        ) \
        inline static void BOOST_LOCAL_AUX_SYMBOL_INIT_CALL_FUNCTION_NAME( \
                void* object, BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_TYPE& functor) { \
            functor.BOOST_LOCAL_AUX_SYMBOL_INIT_CALL_FUNCTION_NAME(object, \
                BOOST_PP_ENUM( \
                        /* PP_INC to handle no dflt (EXPAND for MVSC) */ \
                        BOOST_PP_EXPAND(BOOST_PP_INC(default_count)), \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_STATIC_CALL_FUNC_PTR_, \
                        ~) \
            ); \
        } \
    private: \
        /* run-time: it is faster if call `operator()` just accesses member */ \
        /* references to the ScopeExit struct instead of accessing the bind */ \
        /* struct at each call (these mem refs are init by the constructor) */ \
        BOOST_PP_LIST_FOR_EACH_I( /* const bind member references */ \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_BIND_MEMBER_DECL_,\
                (id, typename_keyword, 0 /* no offset */), \
                const_binds) \
        BOOST_PP_LIST_FOR_EACH_I( /* bind member references */ \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_DECL_, \
                (id, typename_keyword, \
                        /* offset param index of # of preceeding */ \
                        /* const-bindparams (could be 0)*/ \
                        BOOST_PP_LIST_SIZE(const_binds)),\
                binds) \
        /* bind this const or not (pointed-const is not added here because */ \
        /* this is a reference, it is added to the this_ body param instead */ \
        BOOST_PP_EXPR_IIF(BOOST_PP_BITOR(has_bind_this, has_const_bind_this), \
            BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
            & /* all bind member vars are refs to ScopeExit struct members */ \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_MEMBER_THIS_ \
            ; /* end member variable declaration */ \
        ) \
        /* this decl allows for nesting (local functions, etc) as */ \
        /* it makes the args variable visible within the body code (which */ \
        /* cannot be static); this is for compilation only as the args */ \
        /* variable is actually declared by the 1st enclosing local func */ \
        boost::scope_exit::aux::undeclared \
                BOOST_LOCAL_AUX_SYMBOL_ARGS_VARIABLE_NAME; \
        /* body function (unfortunately, cannot be static to allow access */ \
        /* to member var with local function name for recursion but doing */ \
        /* so also allows the body to misuse `this` instead of `this_`) */ \
        inline BOOST_LOCAL_AUX_SYMBOL_RESULT_TYPE(id) \
        BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME( \
                /* const binds */ \
                BOOST_PP_LIST_FOR_EACH_I( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CONST_BIND_DECL_,\
                        (id, typename_keyword, 0 /* no offset */), \
                        const_binds) \
                /* plain binds */ \
                BOOST_PP_COMMA_IF( \
                    BOOST_PP_BITAND( \
                          BOOST_PP_LIST_IS_CONS(const_binds) \
                        , BOOST_PP_LIST_IS_CONS(binds) \
                    ) \
                ) \
                BOOST_PP_LIST_FOR_EACH_I( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_BIND_DECL_, \
                        ( id, typename_keyword, \
                          /* offset param index of # of preceeding */ \
                          /* const-bindparams (could be 0)*/ \
                          BOOST_PP_LIST_SIZE(const_binds) ),\
                        binds) \
                /* `this` bind */ \
                BOOST_PP_COMMA_IF( \
                    BOOST_PP_BITAND( \
                          BOOST_PP_BITOR( \
                              BOOST_PP_LIST_IS_CONS(const_binds) \
                            , BOOST_PP_LIST_IS_CONS(binds) \
                          ) \
                        , BOOST_PP_BITOR(has_const_bind_this, has_bind_this) \
                    ) \
                ) \
                BOOST_PP_EXPR_IIF(has_const_bind_this, \
                    typename_keyword ::boost::local::aux::add_pointed_const< \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
                    >::type \
                    /* const pointer to const object */ \
                    const BOOST_LOCAL_CONFIG_THIS_PARAM_NAME \
                ) \
                BOOST_PP_EXPR_IIF(has_bind_this, \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_BIND_THIS_TYPE(id) \
                    /* const pointer to non-const object */ \
                    const BOOST_LOCAL_CONFIG_THIS_PARAM_NAME \
                ) \
                /* unbind params (last because they can have defaults) */ \
                BOOST_PP_COMMA_IF( \
                    BOOST_PP_BITAND( \
                          BOOST_PP_BITOR( \
                              BOOST_PP_BITOR( \
                                  BOOST_PP_LIST_IS_CONS(const_binds) \
                                , BOOST_PP_LIST_IS_CONS(binds) \
                              ) \
                            , BOOST_PP_BITOR(has_const_bind_this, \
                                    has_bind_this) \
                          ) \
                        , BOOST_PP_LIST_IS_CONS(unbinds) \
                    ) \
                ) \
                BOOST_PP_LIST_FOR_EACH_I( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_UNBIND_DECL_WITH_DEFAULT_, \
                        ~, unbinds) \
            ) /* end body function params */ \
            /* const member func so it cannot change obj (reassign member */ \
            /* var with local function name, etc) */ \
            const \
            /* user local function definition `{ ... }` will follow here */ \
    /* `NAME` macro will close function class decl `};` here */ 

// PUBLIC //

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR( \
        sign_params, id, typename_keyword) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_(sign_params, \
            /* unbind params (might have defaults) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(sign_params), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND_COUNT_DEFAULTS(sign_params), \
            /* const bind vars (without `this`) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_WITHOUT_TYPE( \
                    sign_params), \
            /* eventual const bind `this` */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_CONST_BIND_THIS(sign_params), \
            /* bind (not const) vars (without `this`) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_WITHOUT_TYPE(sign_params), \
            /* eventual bind (not const) `this` */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAVE_BIND_THIS(sign_params), \
            /* etc */ \
            id, typename_keyword)

#endif // #include guard

