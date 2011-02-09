
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HPP_
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HPP_

#include "param.hpp"
#include "../base.hpp"
#include "../../symbol.hpp"
#include "../../preprocessor/sign/params/unbind.hpp"
#include "../../preprocessor/sign/params/const_bind_names.hpp"
#include "../../preprocessor/sign/params/bind_names.hpp"
#include "../../preprocessor/sign/params/has_unbind.hpp"
#include "../../preprocessor/sign/params/has_const_bind.hpp"
#include "../../preprocessor/sign/params/has_bind.hpp"
#include "../../preprocessor/sign/params/has_any_bind.hpp"
#include "../../preprocessor/sign/params/this.hpp"
#include "../../scope_exit/scope_exit.hpp" // Use Boost.ScopeExit internal impl.
#include "../../../config.hpp"
#include "../../../function_ref.hpp"
#include <boost/detail/preprocessor/sign/name.hpp>
#include <boost/detail/preprocessor/sign/result_type.hpp>
#include <boost/detail/preprocessor/sign/inline.hpp>
#include <boost/detail/preprocessor/sign/params/fold_left.hpp>
#include <boost/detail/preprocessor/sign/params/enum.hpp>
#include <boost/detail/preprocessor/sign/params/first_n.hpp>
#include <boost/detail/preprocessor/nilseq.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/logical/bitand.hpp>
#include <boost/preprocessor/logical/and.hpp>
#include <boost/preprocessor/logical/bitor.hpp>
#include <boost/preprocessor/logical/or.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/expr_if.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/type_traits.hpp>

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_DEFAULTS_COUNT_OP_( \
        s, defaults_count, param) \
    BOOST_PP_IIF(BOOST_DETAIL_PP_SIGN_PARAM_HAS_DEFAULT(param), \
        BOOST_PP_INC(defaults_count) \
    , \
        defaults_count \
    )

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_DEFAULTS_COUNT_( \
        params) \
    BOOST_DETAIL_PP_SIGN_PARAMS_FOLD_LEFT( \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_DEFAULTS_COUNT_OP_, \
            0 /* start with defaults_count to 0 */, params)

// Expand to the function type `R (A1, ...)`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_F_(sign) \
    BOOST_DETAIL_PP_SIGN_RESULT_TYPE(sign) \
    ( \
        BOOST_DETAIL_PP_SIGN_PARAMS_ENUM( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_TYPE, ~, \
                BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND( \
                BOOST_DETAIL_PP_SIGN_PARAMS(sign))) \
    )

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_( \
        z, \
        sign, \
        unbind_params, \
        const_bind_names, has_const_bind_this, \
        bind_names, has_bind_this, \
        id) \
    BOOST_DETAIL_PP_SIGN_INLINE(sign) \
    BOOST_DETAIL_PP_SIGN_RESULT_TYPE(sign) operator()( \
            BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_Z(z, \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_DECL, \
                    ~, unbind_params) \
            ) { \
        /* just forward call to member function with local func name */ \
        return BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME( \
            /* this local functor reference (for recursion) */ \
            *this \
            /* pass const binds */ \
            BOOST_PP_COMMA_IF(BOOST_PP_NOT( \
                    BOOST_DETAIL_PP_NILSEQ_IS_NIL(const_bind_names))) \
            BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND, \
                    (id, 0 /* no offset */), \
                    const_bind_names) \
            /* pass plain binds */ \
            BOOST_PP_COMMA_IF(BOOST_PP_NOT( \
                    BOOST_DETAIL_PP_NILSEQ_IS_NIL(bind_names))) \
            BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_MAYBECONST_BIND, \
                    (id \
                    /* offset param index of # of preceeding */ \
                    /* const-bind params (could be 0)*/ \
                    , BOOST_DETAIL_PP_NILSEQ_SIZE(const_bind_names) \
                    ), \
                    bind_names) \
            /* pass bind `this` */ \
            BOOST_PP_COMMA_IF(BOOST_PP_BITOR( \
                    has_const_bind_this, has_bind_this)) \
            BOOST_PP_EXPR_IIF( \
                    BOOST_PP_BITOR(has_const_bind_this, has_bind_this), \
                BOOST_LOCAL_AUX_SYMBOL_PARAMS_VARIABLE_NAME-> \
                BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_NAME_ \
            ) \
            /* pass unbind params */ \
            BOOST_PP_COMMA_IF(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE( \
                    unbind_params)) \
            BOOST_DETAIL_PP_SIGN_PARAMS_ENUM_Z(z, \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_NAME, \
                    ~, unbind_params) \
        ); \
    }

// Return unbind params but without last (default) params specified by count.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_UNBIND_WITHOUT_DEFAULT_( \
        defaults_count, unbind_params) \
    BOOST_DETAIL_PP_SIGN_PARAMS_FIRST_N( \
            BOOST_PP_SUB(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE( \
                    unbind_params), defaults_count), \
            unbind_params)

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_DEFAULTS_( \
        z, n, sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_(z \
        , BOOST_PP_TUPLE_ELEM(7, 0, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
        , BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_UNBIND_WITHOUT_DEFAULT_( \
                n \
                , BOOST_PP_TUPLE_ELEM(7, 1, \
                        sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
                ) \
        , BOOST_PP_TUPLE_ELEM(7, 2, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
        , BOOST_PP_TUPLE_ELEM(7, 3, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
        , BOOST_PP_TUPLE_ELEM(7, 4, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
        , BOOST_PP_TUPLE_ELEM(7, 5, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
        , BOOST_PP_TUPLE_ELEM(7, 6, \
                sign_unbind_constbind_hasconstthis_bind_hasthis_id) \
    )

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HAS_ANY_BIND_( \
        const_bind_names, has_const_bind_this, bind_names, has_bind_this) \
    BOOST_PP_BITOR( \
          BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL(const_bind_names)) \
        , BOOST_PP_BITOR( \
              BOOST_PP_NOT(BOOST_DETAIL_PP_NILSEQ_IS_NIL(bind_names)) \
            , BOOST_PP_BITOR( \
                  has_const_bind_this \
                , has_bind_this \
            ) \
        ) \
    )

// Adapted from `BOOST_SCOPE_EXIT_AUX_IMPL()`.
#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_( \
        sign, \
        unbind_params, defaults_count, \
        const_bind_names, has_const_bind_this, \
        bind_names, has_bind_this, \
        id, typename_keyword) \
    /* typedef outside struct so END doesn't need typename (no END_TPL) */ \
    typedef ::boost::local::function_ref< \
          BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_F_(sign) \
        , defaults_count \
    > BOOST_LOCAL_AUX_SYMBOL_FUNCTION_REF_NAME( \
                        BOOST_DETAIL_PP_SIGN_NAME(sign)); \
    struct BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME( \
            BOOST_DETAIL_PP_SIGN_NAME(sign)) \
        : ::boost::local::aux::function_base< \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_F_(sign), \
                defaults_count> \
    { \
        /* constructor */ \
        explicit BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME( \
                BOOST_DETAIL_PP_SIGN_NAME(sign))(void* bind_params) \
                BOOST_PP_EXPR_IIF( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HAS_ANY_BIND_( \
                                const_bind_names, has_const_bind_this, \
                                bind_names, has_bind_this), \
                    : BOOST_LOCAL_AUX_SYMBOL_PARAMS_VARIABLE_NAME( \
                            (BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)*) /*type-cast*/ \
                            bind_params) \
                ) \
                {} \
        /* implement base functor `operator()` (and for all default params) */ \
        BOOST_PP_REPEAT(BOOST_PP_INC( /* PP_INC to handle also no defaults */ \
                defaults_count), \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_CALL_DEFAULTS_,\
                (sign, unbind_params, const_bind_names, has_const_bind_this, \
                        bind_names, has_bind_this, id) \
                ) \
    private: \
        /* bind params member variable (note -- signle bind param cannot */ \
        /* be represented as single member variables because their names */ \
        /* might be prefixed by `&` so they are not know to pp) */ \
        BOOST_PP_EXPR_IIF( \
                BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_HAS_ANY_BIND_( \
                        const_bind_names, has_const_bind_this, \
                        bind_names, has_bind_this), \
            BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)* \
                    BOOST_LOCAL_AUX_SYMBOL_PARAMS_VARIABLE_NAME; \
        ) \
        BOOST_DETAIL_PP_SIGN_INLINE(sign) \
        static /* static so `this` cannot be mistaken for `this_` */ \
        BOOST_DETAIL_PP_SIGN_RESULT_TYPE(sign) \
        BOOST_LOCAL_AUX_SYMBOL_BODY_FUNCTION_NAME( \
                /* a ref to this local functor for recursion */ \
                BOOST_LOCAL_AUX_SYMBOL_FUNCTOR_CLASS_NAME( \
                        BOOST_DETAIL_PP_SIGN_NAME(sign))& \
                BOOST_DETAIL_PP_SIGN_NAME(sign) \
                /* const binds */ \
                BOOST_PP_COMMA_IF(BOOST_PP_NOT( \
                        BOOST_DETAIL_PP_NILSEQ_IS_NIL(const_bind_names))) \
                BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_CONST_BIND_DECL, \
                        (id, typename_keyword, 0 /* no offset */), \
                        const_bind_names) \
                /* plain binds */ \
                BOOST_PP_COMMA_IF(BOOST_PP_NOT( \
                        BOOST_DETAIL_PP_NILSEQ_IS_NIL(bind_names))) \
                BOOST_DETAIL_PP_NILSEQ_FOR_EACH_I( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_BIND_DECL, \
                        (id, typename_keyword \
                        /* offset param index of # of preceeding */ \
                        /* const-bindparams (could be 0)*/ \
                        , BOOST_DETAIL_PP_NILSEQ_SIZE(const_bind_names) \
                        ), \
                        bind_names) \
                /* `this` bind */ \
                BOOST_PP_COMMA_IF(BOOST_PP_BITOR( \
                        has_const_bind_this, has_bind_this)) \
                BOOST_PP_EXPR_IIF(has_const_bind_this, \
                    typename_keyword ::contract::detail::add_pointed_const< \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_TYPE_(id) \
                    >::type \
                    BOOST_LOCAL_CONFIG_THIS_PARAM_NAME \
                ) \
                BOOST_PP_EXPR_IIF(has_bind_this, \
                    BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_THIS_TYPE_( \
                            id) \
                    BOOST_LOCAL_CONFIG_THIS_PARAM_NAME \
                ) \
                /* unbind params (last because they can have defaults) */ \
                BOOST_PP_COMMA_IF(BOOST_DETAIL_PP_SIGN_PARAMS_SIZE( \
                        unbind_params)) \
                BOOST_DETAIL_PP_SIGN_PARAMS_ENUM( \
                        BOOST_LOCAL_AUX_FUNCTION_CODE_PARAM_UNBIND_DECL_WITH_DEFAULT, \
                        ~, unbind_params) \
            ) /* end body function params */ \
            /* user local function definition `{ ... }` will follow here */ \
    /* `END()` macro will close function class decl `};` here */ 

// Public API.

#define BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR( \
        sign, params, id, typename_keyword) \
    BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_( \
            sign, \
            /* unbind params (may have defaults) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_UNBIND(params), \
            BOOST_LOCAL_AUX_FUNCTION_CODE_FUNCTOR_DEFAULTS_COUNT_( \
                    params), \
            /* const bind param names (may have `this`) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_NAME_REMOVE( \
                    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_CONST_BIND_NAMES( \
                    params)), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_CONST_BIND_THIS( \
                    params), \
            /* (non-const) bind param names (may have `this`) */ \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_THIS_NAME_REMOVE( \
                    BOOST_LOCAL_AUX_PP_SIGN_PARAMS_BIND_NAMES( \
                    params)), \
            BOOST_LOCAL_AUX_PP_SIGN_PARAMS_HAS_BIND_THIS(params), \
            id, typename_keyword)

#endif // #include guard

