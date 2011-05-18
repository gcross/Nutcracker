
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ_HPP_
#define BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ_HPP_

#include <boost/config.hpp>

#if !defined(BOOST_NO_VARIADIC_MACROS) // If no variadics then no macros.

#include "is.hpp"
#include "same.hpp"
#include "eat.hpp"
#include <boost/detail/preprocessor/variadic_macro_data/vmd.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/control/if.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ_NOT_EMPTY_(...) \
    BOOST_PP_IIF(BOOST_LOCAL_AUX_PP_IS_VARIADIC(__VA_ARGS__), \
        BOOST_DETAIL_PP_VMD_DATA_TO_PP_SEQ \
    , \
        BOOST_LOCAL_AUX_PP_VARIADIC_SAME \
    )(__VA_ARGS__)

// PUBLIC //

// Expand to `(arg0)(arg1)...` is __VA_ARGS__ is `arg0, arg1, ...` or already
// `(arg0)(arg1)...`, if __VA_ARGS__ is empty `` expand to `empty_seq`.
#define BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ(empty_seq, ...) \
    BOOST_PP_IF(BOOST_LOCAL_AUX_PP_VARIADIC_SIZE(__VA_ARGS__), \
        BOOST_LOCAL_AUX_PP_VARIADIC_TO_SEQ_NOT_EMPTY_ \
    , \
        empty_seq BOOST_LOCAL_AUX_PP_VARIADIC_EAT \
    )(__VA_ARGS__)

#endif // BOOST_NO_VARIADIC_MACROS

#endif // #include guard


