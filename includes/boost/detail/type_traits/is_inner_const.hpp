
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef CONTRACT_DETAIL_TYPE_TRAITS_IS_INNER_CONST_HPP_
#define CONTRACT_DETAIL_TYPE_TRAITS_IS_INNER_CONST_HPP_

#include <boost/mpl/bool.hpp>

namespace contract { namespace detail {

// Metafunction to check if type has inner const.
// NOTE: `boost::is_const<>` cannot be used instead because it checks just
// for outer `const`.

template<typename T> struct is_inner_const: boost::mpl::false_ {};

// Inner equals outer `const` in this case (there is only one `const`).
template<typename T> struct is_inner_const<T const>: boost::mpl::true_ {};

template<typename T> struct is_inner_const<T&>: boost::mpl::false_ {};

template<typename T> struct is_inner_const<T const&>: boost::mpl::true_ {};

// No specialization for `T& const` because not a valid type.
// No specialization for `T const& const` because not a valid type.

template<typename T> struct is_inner_const<T*>: boost::mpl::false_ {};

template<typename T> struct is_inner_const<T const*>: boost::mpl::true_ {};

template<typename T> struct is_inner_const<T* const>: boost::mpl::false_ {};

template<typename T> struct is_inner_const<T const* const>:
    boost::mpl::true_ {};

}} // namespace

#endif // #include guard

