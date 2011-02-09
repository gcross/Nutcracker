
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_DETAIL_PP_SIGN_AUX_PARAM_INDEXES_HPP_
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_INDEXES_HPP_

// PARAM STRUCTURE
// A parameter is a 5-tuple: (classifier, qualifier, type, name, default)
// * Classifier can be `in`/`out`/`inout` or `auto`/`register` or `PP_EMPTY`.
// * Qualifier can be `copyable` or `PP_EMPTY`.
// * Type can be anything but `PP_EMPTY` -- `auto` or `requires(PREDICATE)` for
//   named parameter "any type" or PREDICATE metafunction type match.
// * Name can be anything but `PP_EMPTY`.
// * Default can be anything or `PP_EMPTY`.

#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_CLASSIFIER_INDEX  0
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_QUALIFIER_INDEX   1
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_TYPE_INDEX        2
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_NAME_INDEX        3
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_DEFAULT_INDEX     4
#define BOOST_DETAIL_PP_SIGN_AUX_PARAM_INDEX_MAX         5

#endif // #include guard

