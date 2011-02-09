
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_EXPORT_HPP_
#define BOOST_DETAIL_PP_SIGN_EXPORT_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"

#define BOOST_DETAIL_PP_SIGN_EXPORT(sign) \
    BOOST_DETAIL_PP_SIGN_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_PARSED_EXPORT_INDEX, sign) \
            (/* expand empty */)

#endif // #include guard

