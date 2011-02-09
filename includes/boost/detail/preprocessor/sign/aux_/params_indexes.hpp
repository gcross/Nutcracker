
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDEXES_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDEXES_HPP_

// Params are represented as 3-array `(3, (tag_namespace, induced, deduced))`
// where:
// * `tag_namespace` is `EMPTY` for no tag (not named params), `EMPTY EMPTY`
//   for named params default tag, and `token EMPTY` for token as named params
//   tag.
// * `induced` are the induced parameters (named or not) as a sequence of NIL
//   and 5-tuples `(NIL) {( ([classifier] EMPTY, [qualifier] EMPTY, type, name,
//   [default] EMPTY) )}*.
// * `deduced` are the deduced parameters (named only) as a sequence of NIL and
//   5-tuples same as the induced params.
// For function parameters: All the above are possible.
// For template parameters: `tag_namespace` is always `EMPTY`, `deduced` is
// always `(NIL)`, `induced` contains the template parameters for which
// `classifer` and `qualifier` are always `EMPTY`.

#define BOOST_DETAIL_PP_SIGN_AUX_PARAMS_TAG_INDEX        0
#define BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDUCED_INDEX    1
#define BOOST_DETAIL_PP_SIGN_AUX_PARAMS_DEDUCED_INDEX    2
#define BOOST_DETAIL_PP_SIGN_AUX_PARAMS_INDEX_MAX        3

#endif // #include guard

