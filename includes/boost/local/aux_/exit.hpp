
// Copyright (C) 2009-2011 Lorenzo Caminiti
// Use, modification, and distribution is subject to the Boost Software
// License, Version 1.0 (see accompanying file LICENSE_1_0.txt or a
// copy at http://www.boost.org/LICENSE_1_0.txt).

#ifndef BOOST_LOCAL_AUX_EXIT_HPP_
#define BOOST_LOCAL_AUX_EXIT_HPP_

#include "symbol.hpp"
#include <boost/function.hpp>
#include <boost/preprocessor/cat.hpp>

// PRIVATE //

#define BOOST_LOCAL_AUX_EXIT_NAME_(name, id) \
    BOOST_LOCAL_AUX_INTERNAL_SYMBOL(BOOST_PP_CAT(BOOST_PP_CAT( \
            BOOST_PP_CAT(exit, BOOST_LOCAL_AUX_SYMBOL_INFIX), name), id))

// PUBLIC //

// Users can exit local scope (and not enclosing function) using `return;`
// (same as with Boost.ScopeExit).
#define BOOST_LOCAL_AUX_EXIT(local_function_params_macro, \
        parenthesized_binding) \
    void local_function_params_macro(parenthesized_binding) \
    /* user code block `{ ... }` here */

#define BOOST_LOCAL_AUX_EXIT_END(id) \
    /* user code exit `{ ... }` here */ \
    /* ends the local function declaration */ \
    BOOST_LOCAL_FUNCTION_NAME(BOOST_LOCAL_AUX_EXIT_NAME_(function, id)) \
    /* guard resourse -- function invoked when guard obj goes out of scope */ \
    ::boost::local::aux::exit_guard BOOST_LOCAL_AUX_EXIT_NAME_(guard, id)( \
            BOOST_LOCAL_AUX_EXIT_NAME_(function, id));

namespace boost { namespace local { namespace aux {

// Resource destructor exec exit code when obj goes out of local scope.
struct exit_guard {
    // Exit local functions are always void with no params (and no defaults).
    typedef ::boost::function<void ()> functor_type;
    explicit exit_guard(functor_type f): f_(f) {}
    ~exit_guard() { f_(); } // Invokes function when obj goes out of scope.
private:
    functor_type f_;
};

}}} // namespace

#endif // #include guard

