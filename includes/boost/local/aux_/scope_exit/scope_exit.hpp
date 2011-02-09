
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

//
// Distributed under the Boost Software License, Version 1.0. 
// (See accompanying file LICENSE_1_0.txt or copy at 
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef FILE_boost_scope_exit_hpp_INCLUDED
#define FILE_boost_scope_exit_hpp_INCLUDED

#include <boost/config.hpp>

#include <boost/detail/workaround.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/seq/cat.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/facilities/identity.hpp>
#include "typeof/typeof.hpp" // Local typeof with `this` MSVC workaround.

#if defined(__GNUC__) && !defined(BOOST_INTEL)
# define BOOST_SCOPE_EXIT_AUX_GCC (__GNUC__ * 100 + __GNUC_MINOR__)
#else
# define BOOST_SCOPE_EXIT_AUX_GCC 0
#endif

#if BOOST_WORKAROUND(BOOST_SCOPE_EXIT_AUX_GCC, BOOST_TESTED_AT(413))
#define BOOST_SCOPE_EXIT_AUX_TPL_WORKAROUND
#endif

// Steven Watanabe's trick with a modification suggested by Kim Barrett
namespace boost { namespace scope_exit { namespace aux {

    // Type of a local boost_scope_exit_args variable.
    // First use in a local scope will declare the boost_scope_exit_args
    // variable, subsequent uses will be resolved as two comparisons
    // (cmp1 with 0 and cmp2 with boost_scope_exit_args).
    template<int Dummy = 0>
    struct declared
    {
        void* value;
        static int const cmp2 = 0;
        friend void operator>(int, declared const&) {}
    };

    struct undeclared { declared<> dummy[2]; };

    template<int> struct resolve;

    template<>
    struct resolve<sizeof(declared<>)>
    {
        static const int cmp1 = 0;
    };

    template<>
    struct resolve<sizeof(undeclared)>
    {
        template<int>
        struct cmp1
        {
            static int const cmp2 = 0;
        };
    };
} } }

extern boost::scope_exit::aux::undeclared boost_scope_exit_args; // undefined


namespace boost { namespace scope_exit { namespace aux {

typedef void (*ref_tag)(int&);
typedef void (*val_tag)(int );

template<class T, class Tag> struct member;

template<class T>
struct member<T,ref_tag>
{
    T& value;
#ifndef BOOST_SCOPE_EXIT_AUX_TPL_WORKAROUND
    member(T& ref) : value(ref) {}
#endif
};

template<class T>
struct member<T,val_tag>
{
    T value;
#ifndef BOOST_SCOPE_EXIT_AUX_TPL_WORKAROUND
    member(T& val) : value(val) {}
#endif
};

template<class T> inline T& deref(T* p, ref_tag) { return *p; }
template<class T> inline T& deref(T& r, val_tag) { return  r; }

template<class T>
struct wrapper
{
    typedef T type;
};

template<class T> wrapper<T> wrap(T&);

} } }

#include BOOST_TYPEOF_INCREMENT_REGISTRATION_GROUP()
BOOST_TYPEOF_REGISTER_TEMPLATE(boost::scope_exit::aux::wrapper, 1)

#define BOOST_SCOPE_EXIT_AUX_GUARD(id)     BOOST_PP_CAT(boost_se_guard_,    id)
#define BOOST_SCOPE_EXIT_AUX_GUARD_T(id)   BOOST_PP_CAT(boost_se_guard_t_,  id)
#define BOOST_SCOPE_EXIT_AUX_PARAMS(id)    BOOST_PP_CAT(boost_se_params_,   id)
#define BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)  BOOST_PP_CAT(boost_se_params_t_, id)

#define BOOST_SCOPE_EXIT_AUX_TAG(id, i) \
    BOOST_PP_SEQ_CAT( (boost_se_tag_)(i)(_)(id) )

#define BOOST_SCOPE_EXIT_AUX_PARAM(id, i, var) \
    BOOST_PP_SEQ_CAT( (boost_se_param_)(i)(_)(id) )

#define BOOST_SCOPE_EXIT_AUX_PARAM_T(id, i, var) \
    BOOST_PP_SEQ_CAT( (boost_se_param_t_)(i)(_)(id) )

#define BOOST_SCOPE_EXIT_AUX_CAPTURE_T(id, i, var) \
    BOOST_PP_SEQ_CAT( (boost_se_capture_t_)(i)(_)(id) )

#define BOOST_SCOPE_EXIT_AUX_WRAPPED(id, i) \
    BOOST_PP_SEQ_CAT( (boost_se_wrapped_t_)(i)(_)(id) )

#define BOOST_SCOPE_EXIT_AUX_DEREF(id, i, var) \
    boost::scope_exit::aux::deref(var, (BOOST_SCOPE_EXIT_AUX_TAG(id,i))0)

#define BOOST_SCOPE_EXIT_AUX_MEMBER(r, id, i, var) \
    boost::scope_exit::aux::member<                \
        BOOST_SCOPE_EXIT_AUX_PARAM_T(id,i,var),    \
        BOOST_SCOPE_EXIT_AUX_TAG(id,i)             \
    > BOOST_SCOPE_EXIT_AUX_PARAM(id,i,var);

// idty is (id,typename) or (id,BOOST_PP_EMPTY())
#define BOOST_SCOPE_EXIT_AUX_ARG_DECL(r, idty, i, var)             \
    BOOST_PP_COMMA_IF(i) BOOST_PP_TUPLE_ELEM(2,1,idty)             \
    BOOST_SCOPE_EXIT_AUX_PARAMS_T(BOOST_PP_TUPLE_ELEM(2,0,idty)):: \
    BOOST_SCOPE_EXIT_AUX_PARAM_T(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var) var
 
#define BOOST_SCOPE_EXIT_AUX_ARG(r, id, i, var) BOOST_PP_COMMA_IF(i) \
    boost_se_params_->BOOST_SCOPE_EXIT_AUX_PARAM(id,i,var).value

#define BOOST_SCOPE_EXIT_AUX_TAG_DECL(r, id, i, var) \
    typedef void (*BOOST_SCOPE_EXIT_AUX_TAG(id,i))(int var);


//----begin-'this-capture'-detection-and-selection-mechanics----

#if BOOST_WORKAROUND(BOOST_MSVC,>=1300)
    // Boost.Typeof for VC71's typeid-based workaround does not work
    // to determine the type of 'this' due to error C2355 being
    // incorrectly reported (a compiler bug that's present in 8 and 9
    // also).  The typical avoidance strategy to evade this error is
    // to make an indirect compile-time constant by assignment through
    // enum, then use that as a type-index.  This cannot happen with
    // the typeid() approach, only with a sizeof() approach.
    // 
    // The implementation below is the more general, type-index
    // sizeof()-based approach implemented in Boost.Typeof which
    // appears to work fine for typeof(this) provided the reference to
    // 'this' is not present in the template argument list (i.e. the
    // type-index is captured into a local enumerator before being
    // used as a template argument).
    //
    // Inline documentation is removed and conditional compilation
    // applied.
    //
    namespace boost { namespace scope_exit { namespace msvc_typeof_this
    {
        template<int ID>
        struct msvc_typeid_wrapper {
            typedef typename boost::type_of::msvc_extract_type<mpl::int_<ID> >::id2type id2type;
            typedef typename id2type::type type;
        };
        template<>
        struct msvc_typeid_wrapper<4> {
            typedef msvc_typeid_wrapper<4> type;
        };
        template<typename T>
        struct encode_type
        {
            BOOST_STATIC_CONSTANT(unsigned,value=BOOST_TYPEOF_INDEX(T));
            typedef typename boost::type_of::msvc_register_type<T,mpl::int_<value> >::id2type type;
            BOOST_STATIC_CONSTANT(unsigned,next=value+1);
            BOOST_TYPEOF_NEXT_INDEX(next);
        };
        template<class T>
        struct sizer
        {
            typedef char(*type)[encode_type<T>::value];
        };
        template<typename T> typename disable_if<
            typename is_function<T>::type,
            typename sizer<T>::type>::type encode_start(T const&);
        template<typename T> typename enable_if<
            typename is_function<T>::type,
            typename sizer<T>::type>::type encode_start(T&);
        template<typename Organizer, typename T>
        boost::type_of::msvc_register_type<T,Organizer> typeof_register_type(const T&,Organizer* =0);

    }}}

# define BOOST_SCOPE_EXIT_TYPEDEF_TYPEOF_THIS() \
    enum { BOOST_PP_CAT(boost_se_thistype_index_,__LINE__) = sizeof(*boost::scope_exit::msvc_typeof_this::encode_start(this)) }; \
    typedef boost::scope_exit::msvc_typeof_this::msvc_typeid_wrapper<BOOST_PP_CAT(boost_se_thistype_index_,__LINE__)>::type

#else // other Boost.Typeof 'this' appear to be okay
# define BOOST_SCOPE_EXIT_TYPEDEF_TYPEOF_THIS() typedef BOOST_TYPEOF(this)
#endif

// 'this-capture' function-macros for when (*this) is requested
//
#define BOOST_SCOPE_EXIT_REALTHIS_COMMA_IF_this(seq)         this BOOST_PP_COMMA_IF(BOOST_PP_SEQ_SIZE(seq))
#define BOOST_SCOPE_EXIT_THISPARAM_COMMA_IF_this(id,seq)     BOOST_PP_CAT(se_this_type,id) se_this BOOST_PP_COMMA_IF(BOOST_PP_SEQ_SIZE(seq))
#define BOOST_SCOPE_EXIT_THISINIT_COMMA_IF_this(seq)         se_this(se_this) BOOST_PP_COMMA_IF(BOOST_PP_SEQ_SIZE(seq))
#define BOOST_SCOPE_EXIT_THISINIT_BEGIN_PARAM_T_INITLIST_this(seq) : 
#define BOOST_SCOPE_EXIT_HASARGS_this(seq)                   1

#define BOOST_SCOPE_EXIT_TYPEDEF_THISTYPE_this(id)           BOOST_SCOPE_EXIT_TYPEDEF_TYPEOF_THIS() BOOST_PP_CAT(se_this_type,id);
#define BOOST_SCOPE_EXIT_DEFINE_THISMEMBER_this(id)          BOOST_PP_CAT(se_this_type,id) se_this;
#define BOOST_SCOPE_EXIT_DEFINE_THIS_ACCESSOR_this(id) \
        inline BOOST_PP_CAT(se_this_type,id) operator->() { return boost_se_params_->se_this; } \
        inline BOOST_PP_CAT(se_this_type,id) operator*()  { return boost_se_params_->se_this; } \
        inline operator BOOST_PP_CAT(se_this_type,id)()   { return boost_se_params_->se_this; } \

// 'this-capture' function-macros for when (*this) is not requested
//
#define BOOST_SCOPE_EXIT_REALTHIS_COMMA_IF_nothis(seq)
#define BOOST_SCOPE_EXIT_THISPARAM_COMMA_IF_nothis(id,seq)
#define BOOST_SCOPE_EXIT_THISINIT_COMMA_IF_nothis(seq)
#define BOOST_SCOPE_EXIT_THISINIT_BEGIN_PARAM_T_INITLIST_nothis(seq) BOOST_PP_IF(BOOST_PP_SEQ_SIZE(seq),BOOST_PP_IDENTITY(:),BOOST_PP_EMPTY)()
#define BOOST_SCOPE_EXIT_HASARGS_nothis(seq)                         BOOST_PP_SEQ_SIZE(seq)

#define BOOST_SCOPE_EXIT_TYPEDEF_THISTYPE_nothis(id)
#define BOOST_SCOPE_EXIT_DEFINE_THISMEMBER_nothis(id)
#define BOOST_SCOPE_EXIT_DEFINE_THIS_ACCESSOR_nothis(id)

// Expand to 1 if the first sequence element is 'this'
// or 0 otherwise.
//
// Mechanics: [given that 'this(x)' --> '(this(x))']
//
//   head = front(seq)  --> 'this'      or 'somearg'
//   funcexp = head(_)  --> '(this(_))' or 'somearg(_)'
//   size_and_possible_tokens = seq_size(funcexp)
//   yields either
//        0 somearg(_)
//   or   1
//   expansion of 'cat(X,size_and_possible_tokens _) )'
//   yields either
//        X0 _ )        --> zero( _ )       --> 0
//   or   X1 tokens _ ) --> one( tokens _ ) --> 1
//
// where X0     is BOOST_SCOPE_EXIT_HAS_THIS_CONSUME_0
//       X1     is BOOST_SCOPE_EXIT_HAS_THIS_CONSUME_1
//       zero() is BOOST_SCOPE_EXIT_HAS_THIS_FALSE()
//       one()  is BOOST_SCOPE_EXIT_HAS_THIS_TRUE()
//
#define BOOST_SCOPE_EXIT_HAS_THIS(seq)                            \
   BOOST_PP_CAT(BOOST_SCOPE_EXIT_HAS_THIS_CONSUME_,               \
                BOOST_PP_SEQ_SIZE(BOOST_PP_SEQ_HEAD(seq)(_)) _) ) \

#define BOOST_SCOPE_EXIT_HAS_THIS_CONSUME_0 BOOST_SCOPE_EXIT_HAS_THIS_FALSE(
#define BOOST_SCOPE_EXIT_HAS_THIS_CONSUME_1 BOOST_SCOPE_EXIT_HAS_THIS_TRUE(

#define BOOST_SCOPE_EXIT_HAS_THIS_FALSE(x) 0
#define BOOST_SCOPE_EXIT_HAS_THIS_TRUE(x)  1

// Forwarding front-end to determine whether 'this' is the first
// element of the sequence and adapt the invocation of the detail
// accordingly.  The dummy is passed to allow for an empty sequence.
#define BOOST_SCOPE_EXIT_AUX_IMPL_FE(id, seq, ty)                                     \
   BOOST_SCOPE_EXIT_AUX_IMPL_FE2(id, seq, ty, BOOST_SCOPE_EXIT_HAS_THIS(seq (dummy))) \

#define BOOST_SCOPE_EXIT_AUX_IMPL_FE2(id, seq, ty, has_this)                        \
   BOOST_SCOPE_EXIT_AUX_IMPL(id, BOOST_PP_IF(has_this, BOOST_PP_SEQ_TAIL(seq),seq), \
                             ty, BOOST_PP_IF(has_this, this,nothis) )               \

//----end-'this-capture'-detection-and-selection-mechanics----

#ifdef BOOST_SCOPE_EXIT_AUX_TPL_WORKAROUND

#define BOOST_SCOPE_EXIT_AUX_PARAMS_T_CTOR(id, seq, this)

#define BOOST_SCOPE_EXIT_AUX_PARAM_INIT(r, id, i, var) \
    BOOST_PP_COMMA_IF(i) { BOOST_SCOPE_EXIT_AUX_DEREF(id,i,var) }

#define BOOST_SCOPE_EXIT_AUX_PARAMS_INIT(id, seq, this) \
    = { BOOST_PP_CAT(BOOST_SCOPE_EXIT_REALTHIS_COMMA_IF_,this)(seq) \
        BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_PARAM_INIT, id, seq) };

#else

#define BOOST_SCOPE_EXIT_AUX_CTOR_ARG(r, id, i, var) BOOST_PP_COMMA_IF(i) \
    BOOST_SCOPE_EXIT_AUX_PARAM_T(id,i,var) & BOOST_PP_CAT(a,i)

#define BOOST_SCOPE_EXIT_AUX_MEMBER_INIT(r, id, i, var) BOOST_PP_COMMA_IF(i) \
    BOOST_SCOPE_EXIT_AUX_PARAM(id,i,var) ( BOOST_PP_CAT(a,i) )

#define BOOST_SCOPE_EXIT_AUX_PARAMS_T_CTOR(id, seq, this)                  \
    BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)(                                     \
        BOOST_PP_CAT(BOOST_SCOPE_EXIT_THISPARAM_COMMA_IF_,this)(id,seq)    \
        BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_CTOR_ARG, id, seq ) ) \
      BOOST_PP_CAT(BOOST_SCOPE_EXIT_THISINIT_BEGIN_PARAM_T_INITLIST_,this)(seq)\
      BOOST_PP_CAT(BOOST_SCOPE_EXIT_THISINIT_COMMA_IF_,this)(seq)          \
      BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_MEMBER_INIT, id, seq) {}

#define BOOST_SCOPE_EXIT_AUX_PARAM_INIT(r, id, i, var) \
    BOOST_PP_COMMA_IF(i) BOOST_SCOPE_EXIT_AUX_DEREF(id,i,var)

#define BOOST_SCOPE_EXIT_AUX_PARAMS_INIT(id, seq, this)                    \
    BOOST_PP_IF(BOOST_PP_CAT(BOOST_SCOPE_EXIT_HASARGS_,this)(seq),         \
    BOOST_PP_IDENTITY(                                                     \
    ( BOOST_PP_CAT(BOOST_SCOPE_EXIT_REALTHIS_COMMA_IF_,this)(seq)          \
      BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_PARAM_INIT, id, seq) )  \
    ),BOOST_PP_EMPTY)();                                                   \

#endif

#if defined(BOOST_TYPEOF_EMULATION)

#define BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL(r, idty, i, var)                   \
    struct BOOST_SCOPE_EXIT_AUX_WRAPPED(BOOST_PP_TUPLE_ELEM(2,0,idty), i)    \
        : BOOST_TYPEOF(boost::scope_exit::aux::wrap(                         \
        BOOST_SCOPE_EXIT_AUX_DEREF(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var)))  \
    {}; typedef BOOST_PP_TUPLE_ELEM(2,1,idty)                                \
        BOOST_SCOPE_EXIT_AUX_WRAPPED(BOOST_PP_TUPLE_ELEM(2,0,idty), i)::type \
        BOOST_SCOPE_EXIT_AUX_CAPTURE_T(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var);

#elif defined(BOOST_INTEL)

#define BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL(r, idty, i, var)                 \
    typedef BOOST_TYPEOF_KEYWORD(                                          \
        BOOST_SCOPE_EXIT_AUX_DEREF(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var)) \
        BOOST_SCOPE_EXIT_AUX_CAPTURE_T(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var);

#else

#define BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL(r, idty, i, var)                   \
    typedef BOOST_TYPEOF(boost::scope_exit::aux::wrap(                       \
        BOOST_SCOPE_EXIT_AUX_DEREF(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var)))  \
        BOOST_SCOPE_EXIT_AUX_WRAPPED(BOOST_PP_TUPLE_ELEM(2,0,idty), i);      \
    typedef BOOST_PP_TUPLE_ELEM(2,1,idty)                                    \
        BOOST_SCOPE_EXIT_AUX_WRAPPED(BOOST_PP_TUPLE_ELEM(2,0,idty), i)::type \
        BOOST_SCOPE_EXIT_AUX_CAPTURE_T(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var);

#endif

#define BOOST_SCOPE_EXIT_AUX_PARAM_DECL(r, idty, i, var) \
    typedef BOOST_SCOPE_EXIT_AUX_CAPTURE_T(              \
        BOOST_PP_TUPLE_ELEM(2,0,idty), i, var)           \
        BOOST_SCOPE_EXIT_AUX_PARAM_T(BOOST_PP_TUPLE_ELEM(2,0,idty), i, var);


#define BOOST_SCOPE_EXIT_AUX_IMPL(id, seq, ty, this)                           \
    BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_TAG_DECL, id, seq)            \
    BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_CAPTURE_DECL, (id,ty), seq)   \
    BOOST_PP_CAT(BOOST_SCOPE_EXIT_TYPEDEF_THISTYPE_,this)(id)                  \
    struct BOOST_SCOPE_EXIT_AUX_PARAMS_T(id) {                                 \
        BOOST_PP_CAT(BOOST_SCOPE_EXIT_DEFINE_THISMEMBER_,this)(id)             \
        BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_PARAM_DECL, (id,ty), seq) \
        BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_MEMBER, id, seq)          \
        BOOST_SCOPE_EXIT_AUX_PARAMS_T_CTOR(id, seq, this)                      \
    } BOOST_SCOPE_EXIT_AUX_PARAMS(id) BOOST_SCOPE_EXIT_AUX_PARAMS_INIT(id, seq, this)\
    boost::scope_exit::aux::declared< boost::scope_exit::aux::resolve<         \
        sizeof(boost_scope_exit_args)>::cmp1<0>::cmp2 > boost_scope_exit_args; \
    boost_scope_exit_args.value = &BOOST_SCOPE_EXIT_AUX_PARAMS(id);            \
    struct BOOST_SCOPE_EXIT_AUX_GUARD_T(id) {                                  \
        BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)* boost_se_params_;                   \
        BOOST_SCOPE_EXIT_AUX_GUARD_T(id) (void* boost_se_params)               \
            : boost_se_params_(                                                \
                  (BOOST_SCOPE_EXIT_AUX_PARAMS_T(id)*)boost_se_params)         \
        {}                                                                     \
        BOOST_PP_CAT(BOOST_SCOPE_EXIT_DEFINE_THIS_ACCESSOR_,this)(id)          \
        ~BOOST_SCOPE_EXIT_AUX_GUARD_T(id)() { boost_se_body(                   \
            BOOST_PP_SEQ_FOR_EACH_I(BOOST_SCOPE_EXIT_AUX_ARG, id, seq) ); }    \
        void boost_se_body(BOOST_PP_SEQ_FOR_EACH_I(                            \
            BOOST_SCOPE_EXIT_AUX_ARG_DECL, (id,ty), seq) )

#if defined(BOOST_MSVC)

#ifndef BOOST_SCOPE_EXIT_DONT_DISABLE_C4003
#pragma warning(disable:4003)
#endif

#define BOOST_SCOPE_EXIT_END } BOOST_SCOPE_EXIT_AUX_GUARD(__COUNTER__) ( \
    boost_scope_exit_args.value);

#define BOOST_SCOPE_EXIT(seq) \
    BOOST_SCOPE_EXIT_AUX_IMPL_FE(__COUNTER__, seq, BOOST_PP_EMPTY())

#else

#define BOOST_SCOPE_EXIT_END } BOOST_SCOPE_EXIT_AUX_GUARD(__LINE__) ( \
    boost_scope_exit_args.value);

#define BOOST_SCOPE_EXIT(seq) \
    BOOST_SCOPE_EXIT_AUX_IMPL_FE(__LINE__, seq, BOOST_PP_EMPTY())

#endif

#ifdef BOOST_SCOPE_EXIT_AUX_TPL_WORKAROUND
#define BOOST_SCOPE_EXIT_TPL(seq) \
    BOOST_SCOPE_EXIT_AUX_IMPL_FE(__LINE__, seq, typename)
#else
#define BOOST_SCOPE_EXIT_TPL(seq) BOOST_SCOPE_EXIT(seq)
#endif

#endif // #ifndef FILE_boost_scope_exit_hpp_INCLUDED

