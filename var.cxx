#include <array>
#include <tuple>
#include <cstdio>
#include <iostream>
#include <type_traits>
#include <utility>
#include <experimental/array>

namespace std { using namespace experimental; }

// template <typename ...Ts> inline constexpr
// auto every(Ts && ...ts) {
  
// }

template <typename T> struct dependent_false: std::false_type {};

template <typename CharT, CharT ...Cs>
struct string_literal {
  static constexpr std::size_t const size = sizeof... (Cs);
  using type = CharT;

  constexpr inline auto tuple() const {
    return std::make_tuple(Cs...);
  }
  constexpr inline auto array() const {
    return std::make_array(Cs...);
  }
};

template <typename CharT, std::size_t N>
struct string_array {
  static constexpr std::size_t const size = N;
  using type = CharT;

  constexpr string_array(CharT const (&data)[N])
  : data_{data} {}

  constexpr inline auto tuple() const {
    return tuple_impl_(data_);
  }
  constexpr inline auto array() const {
    return std::to_array(data_);
  }

 private:
  template <std::size_t ...Is>
  constexpr inline auto tuple_impl_(std::index_sequence<Is...>) const {
    return std::make_tuple(data_[Is]...);
  }

  CharT const (&data_)[N];
};

template <typename CharT, std::size_t N>
string_array(CharT (&)[N]) -> string_array<CharT, N>;

template <typename T>
string_array(T &&) -> string_array<std::remove_pointer_t<std::decay_t<T>>,
                                   std::extent_v<std::remove_reference_t<T>>>;

template <typename T, std::size_t N, std::size_t ...Is>
auto nullify_array_impl(std::array<T, N> arr, std::index_sequence<Is...>) {
  return std::make_array(std::get<Is>(arr)..., '\0');
}

template <typename T, std::size_t N>
auto nullify_array(std::array<T, N> arr) {
  return nullify_array_impl(arr, std::make_index_sequence<N>{});
}

template <typename> struct is_tuple: std::false_type {};
template <typename ...T> struct is_tuple<std::tuple<T...>>: std::true_type {};
template <typename T> inline constexpr bool is_tuple_v = is_tuple<T>::value;
template <typename T> using is_tuple_t = typename is_tuple<T>::type;

template <typename> struct is_string_literal: std::false_type {};
template <typename CharT, CharT ...Cs> struct is_string_literal<string_literal<CharT, Cs...>>: std::true_type {};
template <typename T> inline constexpr bool is_string_literal_v = is_string_literal<T>::value;
template <typename T> using is_string_literal_t = typename is_string_literal<T>::type;

namespace details {

template <typename T, std::size_t N, std::size_t ...Is>
constexpr std::array<std::remove_cv_t<T>, N> to_array_impl(T (&t)[N], std::index_sequence<Is...>) {
  return {{t[Is]...}};
}

template <typename T, std::size_t N, std::size_t ...Is>
constexpr auto to_tuple_impl(T (&t)[N], std::index_sequence<Is...>) {
  return std::make_tuple(static_cast<std::remove_cv_t<T>>(t[Is])...);
}

}

template <typename T, std::size_t N>
constexpr std::array<std::remove_cv_t<T>, N> to_array(T (&t)[N])
noexcept (std::is_nothrow_constructible<std::remove_cv_t<T>, T &>::value) {
  return details::to_array_impl(t, std::make_index_sequence<N>{});
}

template <typename T, std::size_t N>
constexpr auto to_tuple(T (&t)[N])
noexcept (std::is_nothrow_constructible<std::remove_cv_t<T>, T &>::value) {
  return details::to_tuple_impl(t, std::make_index_sequence<N>{});
}

template<class CharT, CharT ...Cs>
constexpr auto operator ""_lit() { return string_literal<CharT, Cs..., 0>{}; }

namespace details {

template <typename Arg>
constexpr void validate_integer() {
  static_assert(std::is_integral_v<std::remove_reference_t<Arg>>,
                "Mismatch for an int argument; must be %d");
}

template <typename Arg>
constexpr void validate_string() {
  static_assert(std::is_convertible_v<char const *, std::decay_t<Arg>>,
                "Mismatch for a string argument; must be %s");
}

template <typename Arg>
constexpr void validate_unknown() {
  static_assert(dependent_false<Arg>{}, "Unsupported %-specifier");
}

template <typename Fmt, std::size_t N, std::size_t FmtI, typename Arg = void, typename ...Args>
struct validate_helper {
  constexpr bool loop(Fmt &&fmt) && {
    static_assert(!(FmtI == N-1 && sizeof... (Args) > 0), "Extra parameters");

    // if constexpr (std::get<FmtI>(fmt.array()) == '\0' && std::is_void_v<Arg> && sizeof... (Args)) {
    //   return true;
    // }

    if constexpr (std::get<FmtI>(fmt.array()) == '%') {
      static_assert(std::bool_constant<FmtI + 1 < N>{}, "Incorrect format string");

      if constexpr (std::get<FmtI + 1>(fmt.array()) == '%') {
        return validate_helper<Fmt, N, FmtI + 2, Arg, Args...>{}.loop(std::move(fmt));
      } else if constexpr (std::get<FmtI + 1>(fmt.array()) == 'd') {
        validate_integer<Arg>();
        return validate_helper<Fmt, N, FmtI + 2, Args...>{}.loop(std::move(fmt));
      } else if constexpr (std::get<FmtI + 1>(fmt.array()) == 's') {
        validate_string<Arg>();
        return validate_helper<Fmt, N, FmtI + 2, Args...>{}.loop(std::move(fmt));
      } else {
        validate_unknown<Arg>();
        return false;
      }
    }

    return validate_helper<Fmt, N, FmtI + 1, Arg, Args...>{}.loop(std::move(fmt));
  }
};

template <typename Fmt, std::size_t N, typename Arg, typename ...Args>
struct validate_helper<Fmt, N, N, Arg, Args...> {
  constexpr bool loop(Fmt &&) && {
    return true;
  }
};

template <typename Fmt, std::size_t N, typename ...Args>
constexpr bool validate(Fmt &&fmt) {
  return validate_helper<Fmt, N, 0, Args...>{}.loop(std::move(fmt));
}

template <typename Fmt, typename ...Args>
constexpr bool validate_static(Fmt &&fmt) {
  constexpr std::size_t const N = Fmt::size;
  return validate<Fmt, N, Args...>(std::move(fmt));
}

}

template <typename Fmt, typename ...Args>
constexpr void print(Fmt &&fmt, Args && ...args);

template <typename Fmt, typename ...Args>
requires std::is_array_v<std::remove_reference_t<Fmt>>
constexpr void print [[deprecated("arrays aren't supported")]] (Fmt &&fmt, Args && ...args) {
  // auto const &&raw = string_array<std::remove_pointer_t<std::decay_t<Fmt>>,
  //                                 std::extent_v<std::remove_reference_t<Fmt>>>{fmt}
  //                    .array().data();
  // auto const &&raw = arr.array().data();
  // auto const &&arr = string_array{fmt};
  // constexpr bool const valid = validate<decltype(string_array{fmt}), Args...>(string_array{fmt});
  // ::printf("static validation says %s\n", valid ? "yes" : "no");
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("WARN: arrays aren't supported\n");
  ::printf(fmt, args...);
}

template <typename Fmt, typename ...Args>
requires std::is_pointer_v<std::remove_reference_t<Fmt>> &&
         std::is_convertible_v<char const *, std::decay_t<Fmt>>
constexpr void print [[deprecated("can't validate a dynamic format string")]] (Fmt &&fmt, Args && ...args) {
  // constexpr bool const valid = validate<Fmt, Args...>(fmt);
  // ::printf("dynamic valid=%d\n", int{valid});
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("WARN: can't validate a dynamic format string\n");
  ::printf(fmt, args...);
}

template <typename Fmt, typename ...Args>
requires is_string_literal_v<Fmt>
constexpr void print(Fmt &&fmt, Args && ...args) {
  auto const &&raw = fmt.array().data();
  constexpr bool const valid = details::validate_static<Fmt, Args...>(std::move(fmt));
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("INFO: static validation says %s\n", valid ? "yes" : "no");
  ::printf(raw, args...);
}

int main(int argc, char *argv[]) {
  int i = 42;
  print("integer %d\n"_lit, i);

  char const *fmt1 = "integer is %d\n";
  print(fmt1, i);

  char const fmt2[] = "integer is %d\n";
  print(fmt2, i);
  print("integer is %d\n", i);
}
