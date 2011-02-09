
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef CONTRACT_DETAIL_POINTER_TRAITS_HPP_
#define CONTRACT_DETAIL_POINTER_TRAITS_HPP_

namespace contract { namespace detail {

// Metafunction to add const to pointed type `T` (i.e. converts
// `T* [const]` to `T const* [const]`).
// NOTE: `boost::add_const<>` cannot be used instead because only adds outer
// const.

template<typename T> struct add_pointed_const { typedef T type; };

template<typename T> struct add_pointed_const<T*> { typedef T const* type; };

template<typename T> struct add_pointed_const<T const*>
    { typedef T const* type; };

template<typename T> struct add_pointed_const<T* const>
    { typedef T const* const type; };

template<typename T> struct add_pointed_const<T const* const>
    { typedef T const* const type; };

}} // namespace

#endif //#include guard

