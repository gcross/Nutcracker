
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#if !BOOST_PP_IS_ITERATING
#   ifndef BOOST_LOCAL_AUX_OVERLOAD_BASE_HPP_
#       define BOOST_LOCAL_AUX_OVERLOAD_BASE_HPP_

#       include "file.hpp"
#       include "../config.hpp"
#       include <boost/function.hpp>
#       include <boost/preprocessor/iteration/iterate.hpp>
#       include <boost/preprocessor/repetition/enum.hpp>
#       include <boost/preprocessor/cat.hpp>
#       include <boost/preprocessor/comma_if.hpp>

#define BOOST_LOCAL_AUX_arg_type(z, n, unused) \
    BOOST_PP_CAT(A, n)

#define BOOST_LOCAL_AUX_arg_name(z, n, unused) \
    BOOST_PP_CAT(a, n)

#define BOOST_LOCAL_AUX_arg_tparam(z, n, unused) \
    typename BOOST_LOCAL_AUX_arg_type(z, n, unused)

#define BOOST_LOCAL_AUX_arg(z, n, unused) \
    BOOST_LOCAL_AUX_arg_type(z, n, unused) \
    BOOST_LOCAL_AUX_arg_name(z, n, unused)

#define BOOST_LOCAL_AUX_f \
    R (BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity, BOOST_LOCAL_AUX_arg_type, ~))

namespace boost { namespace local { namespace aux {

template<typename F>
class overload_base {
    // Empty template cannot be used directly (only via its specializations).
};

#       define BOOST_PP_ITERATION_PARAMS_1 \
                (3, (0, BOOST_LOCAL_CONFIG_FUNCTION_ARITY_MAX, \
                BOOST_LOCAL_AUX_FILE_OVERLOAD_BASE_HPP))
#       include BOOST_PP_ITERATE() // Iterate over funciton arity.

}}} // namespace boost::local::aux

#undef BOOST_LOCAL_AUX_arg_type
#undef BOOST_LOCAL_AUX_arg_name
#undef BOOST_LOCAL_AUX_arg_tparam
#undef BOOST_LOCAL_AUX_arg
#undef BOOST_LOCAL_AUX_f

#   endif // #include guard

#elif BOOST_PP_ITERATION_DEPTH() == 1
#   define BOOST_LOCAL_AUX_arity BOOST_PP_FRAME_ITERATION(1)

// Iterating within namespace boost::local::aux.
template<typename R
    BOOST_PP_COMMA_IF(BOOST_LOCAL_AUX_arity)
    BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity, BOOST_LOCAL_AUX_arg_tparam, ~)
>
class overload_base<BOOST_LOCAL_AUX_f> {
public:
    /* implicit */ inline overload_base(
            // This requires specified type to be implicitly convertible to
            // a boost::function<> functor.
            ::boost::function<BOOST_LOCAL_AUX_f> const& f): f_(f)
    {}

    inline R operator()(BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity,
            BOOST_LOCAL_AUX_arg, ~)) const {
        return f_(BOOST_PP_ENUM(BOOST_LOCAL_AUX_arity,
                BOOST_LOCAL_AUX_arg_name, ~));
    }

private:
    ::boost::function<BOOST_LOCAL_AUX_f> const f_;
};

#   undef BOOST_LOCAL_AUx_arity
#endif // iteration

