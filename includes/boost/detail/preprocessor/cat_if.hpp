
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_CAT_IF_HPP_
#define BOOST_DETAIL_PP_CAT_IF_HPP_

#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/logical/bool.hpp>

#define BOOST_DETAIL_PP_CAT_IF_YES_(left, right, nocat) \
    BOOST_PP_CAT(left, right)

#define BOOST_DETAIL_PP_CAT_IF_NO_(left, right, nocat) \
    nocat

#define BOOST_DETAIL_PP_CAT_IIF_(condition01, left, right, nocat) \
    BOOST_PP_IIF(condition01, \
        BOOST_DETAIL_PP_CAT_IF_YES_ \
    , \
        BOOST_DETAIL_PP_CAT_IF_NO_ \
    )(left, right, nocat)

// Public API.

// If `condition` then `left ## right` else `left`.
#define BOOST_DETAIL_PP_CAT_IIF(condition01, left, right) \
    BOOST_DETAIL_PP_CAT_IIF_(condition01, left, right, left)

// If `condition` then `left ## right` else `right`.
#define BOOST_DETAIL_PP_CAT_IIF_RIGHT(condition01, left, right) \
    BOOST_DETAIL_PP_CAT_IIF_(condition01, left, right, right)

// If `condition` then `left ## right` else `left`.
#define BOOST_DETAIL_PP_CAT_IF(condition, left, right) \
    BOOST_DETAIL_PP_CAT_IIF_(BOOST_PP_BOOL(condition), left, right, left)

// If `condition` then `left ## right` else `right`.
#define BOOST_DETAIL_PP_CAT_IF_RIGHT(condition01, left, right) \
    BOOST_DETAIL_PP_CAT_IIF_(BOOST_PP_BOOL(condition), left, right, right)

#endif // #include guard

