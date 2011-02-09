
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_BODY_HPP_
#define BOOST_DETAIL_PP_SIGN_BODY_HPP_

#include "aux_/parsed/elem.hpp"
#include "aux_/parsed/indexes.hpp"

/** @todo To improve performances, consider removing trailing EMPTY for req signature traits -- but would removing EMPTY really improve performances? For example, body should always be there (either as {}, or ;, or = 0;, etc) so EMPTY is not needed here. Check all traits that only have a _REQ macro so they cannot be optional _OPT or none _NO and they can omit trailing EMPTY for efficiency. */

#define BOOST_DETAIL_PP_SIGN_BODY(sign) \
    BOOST_DETAIL_PP_SIGN_AUX_PARSED_ELEM( \
            BOOST_DETAIL_PP_SIGN_AUX_BODY_INDEX, sign) 

#endif // #include guard

