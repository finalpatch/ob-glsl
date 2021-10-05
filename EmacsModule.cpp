#include "EmacsModule.hpp"

namespace detail {

template<>
std::string extractArg<std::string>(emacs_env *env, emacs_value v) {
    ptrdiff_t len = 0;
    env->copy_string_contents(env, v, nullptr, &len);
    std::string s(len-1, ' ');
    env->copy_string_contents(env, v, s.data(), &len);
    return s;
}

template<>
int extractArg<int>(emacs_env *env, emacs_value v) {
    return env->extract_integer(env, v);
}
}

static std::map<std::string, emacs_value> symCache;

emacs_value sym(emacs_env *env, const std::string& name) {
    auto i = symCache.find(name);
    if (i != symCache.end()) {
        return i->second;
    }
    auto localRef = env->intern(env, name.data());
    auto globalRef = env->make_global_ref(env, localRef);
    if (!localRef || !globalRef) throw std::runtime_error("failed to intern symbol: " + name);
    symCache.emplace(name, globalRef);
    return globalRef;
}

emacs_value reportError(emacs_env *env, const std::exception& e) {
    emacs_value Qerr = env->intern(env, "native-exception");
    std::string msg = e.what();
    emacs_value Qmsg = env->make_string(env, msg.data(), msg.size());
    emacs_value Qnil = env->intern(env, "nil");
    emacs_value Qdata = env->funcall(env, env->intern(env, "list"), 1, &Qmsg);
    env->non_local_exit_signal(env, Qerr, Qdata);
    return Qnil;
}

void provide(emacs_env *env, const char *feature) {
    emacs_value Qfeat = env->intern(env, feature);
    emacs_value Qprovide = env->intern(env, "provide");
    env->funcall(env, Qprovide, 1, &Qfeat);
}

int plugin_is_GPL_compatible;
