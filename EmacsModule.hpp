#pragma once

#include "emacs-module.h"
#include <string>
#include <map>
#include <tuple>
#include <stdexcept>

namespace detail {

template<typename T>
T extractArg(emacs_env *env, emacs_value v);
template<> std::string extractArg<std::string>(emacs_env *env, emacs_value v);
template<> int extractArg<int>(emacs_env *env, emacs_value v);

template <typename...ARGS, std::size_t... I>
auto extractArgsImpl(emacs_env *env, emacs_value* args, std::index_sequence<I...>) {
    return std::make_tuple(extractArg<ARGS>(env, args[I])...);
}

template <typename...ARGS>
auto extractArgs(emacs_env *env, ptrdiff_t nargs, emacs_value* args) {
    using indices = std::make_index_sequence<sizeof...(ARGS)>;
    return extractArgsImpl<std::decay_t<ARGS>...>(env, args, indices{});
}
}

emacs_value sym(emacs_env *env, const std::string& name);

emacs_value reportError(emacs_env *env, const std::exception& e);

void provide(emacs_env *env, const char *feature);

template <class... ARGS>
void bindFunction(emacs_env *env, emacs_value f(emacs_env*, ARGS...), const char* name) {
    auto numArgs = sizeof...(ARGS);
    auto trampoline = [] (emacs_env *env, ptrdiff_t nargs, emacs_value* args, void *data) noexcept {
        auto func = reinterpret_cast<decltype(f)>(data);
        auto argsTuple = detail::extractArgs<ARGS...>(env, nargs, args);
        try {
            return std::apply(func, std::tuple_cat(std::make_tuple(env), argsTuple));
        } catch (std::exception& e) {
            return reportError(env, e);
        }
    };
    emacs_value Sfun = env->make_function(env, numArgs, numArgs, trampoline, name, reinterpret_cast<void*>(f));
    emacs_value Qfset = sym(env, "fset");
    emacs_value Qsym = env->intern(env, name);
    emacs_value args[] = {Qsym, Sfun};
    env->funcall(env, Qfset, 2, args);
}
