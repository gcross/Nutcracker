
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the
// Boost Software License, Version 1.0
// (see accompanying file LICENSE_1_0.txt or a copy at
// http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_EXIT_HPP_
#define BOOST_LOCAL_AUX_EXIT_HPP_

#include "symbol.hpp"
#include "../function_ref.hpp"
#include <boost/preprocessor/cat.hpp>

#define BOOST_LOCAL_AUX_EXIT_NAME(name) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            BOOST_PP_CAT(exit, BOOST_LOCAL_AUX_SYMBOL_INFIX), \
            name), __LINE__))

// Users can exit local scope (and not enclosing function) using `return;`
// (same as with Boost.ScopeExit).
#define BOOST_LOCAL_AUX_EXIT(local_function_macro, \
        parenthesized_binding) \
    local_function_macro( \
    (void) (BOOST_LOCAL_AUX_EXIT_NAME(function))( \
            parenthesized_binding ) \
    ) /* user code block `{ ... }` here */

namespace boost { namespace local { namespace aux {

// Resource destructor exec exit code when obj goes out of local scope.
struct exit_guard {
    // Exit local functions are always void with no params (and no defaults).
    typedef ::boost::local::function_ref<void ()> ref_type;
    explicit exit_guard(ref_type ref): ref_(ref) {}
    ~exit_guard() { ref_(); }
private:
    ref_type ref_;
};

}}} // namespace boost::local::aux

#endif // #include guard

