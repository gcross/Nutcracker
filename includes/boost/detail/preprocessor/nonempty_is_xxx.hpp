
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_NONEMPTY_IS_XXX_HPP_
#define BOOST_DETAIL_PP_NONEMPTY_IS_XXX_HPP_

#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/tuple/eat.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>

// Must use extra macro to avoid passign empty macro param.
#define BOOST_DETAIL_PP_NONEMPTY_IS_XXX_(tokens, get_macro, is_xxx_macro) \
    is_xxx_macro(get_macro(tokens))

// If `get(tokens) != PP_EMPTY` && `is_xxx(get(tokens))` then 1, else 0.
#define BOOST_DETAIL_PP_NONEMPTY_IS_XXX(tokens, get_macro, is_xxx_macro) \
    BOOST_PP_IIF(BOOST_PP_IS_EMPTY(get_macro(tokens)), \
        0 /* empty so expand to false */ \
        BOOST_PP_TUPLE_EAT(3) \
    , \
        BOOST_DETAIL_PP_NONEMPTY_IS_XXX_ /* non-empty so check if xxx */ \
    )(tokens, get_macro, is_xxx_macro)

#endif // #include guard

