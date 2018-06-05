#include <array>
#include <tuple>
#include <cstdio>
#include <iostream>
#include <type_traits>
#include <utility>
#include <experimental/array>

namespace std { using namespace experimental; }

template <typename T> struct dependent_false: std::false_type {};

template <typename CharT, CharT ...Cs>
struct string_literal {
  static constexpr std::size_t const size = sizeof... (Cs);
  using element_type = CharT;

  constexpr inline auto array() const {
    return std::make_array(Cs...);
  }
};

template <typename T, std::size_t N, std::size_t ...Is>
auto nullify_array_impl(std::array<T, N> arr, std::index_sequence<Is...>) {
  return std::make_array(std::get<Is>(arr)..., '\0');
}

template <typename T, std::size_t N>
auto nullify_array(std::array<T, N> arr) {
  return nullify_array_impl(arr, std::make_index_sequence<N>{});
}

template <typename> struct is_string_literal: std::false_type {};
template <typename CharT, CharT ...Cs> struct is_string_literal<string_literal<CharT, Cs...>>: std::true_type {};
template <typename T> inline constexpr bool is_string_literal_v = is_string_literal<T>::value;
template <typename T> using is_string_literal_t = typename is_string_literal<T>::type;

namespace details {

template <typename T, std::size_t N, std::size_t ...Is>
constexpr std::array<std::remove_cv_t<T>, N> to_array_impl(T (&t)[N], std::index_sequence<Is...>) {
  return {{t[Is]...}};
}

}

template <typename T, std::size_t N>
constexpr std::array<std::remove_cv_t<T>, N> to_array(T (&t)[N])
noexcept (std::is_nothrow_constructible<std::remove_cv_t<T>, T &>::value) {
  return details::to_array_impl(t, std::make_index_sequence<N>{});
}

template<class CharT, CharT ...Cs>
constexpr auto operator ""_lit() { return string_literal<CharT, Cs..., 0>{}; }

namespace details {

template <typename Arg>
constexpr void validate_integer() {
  static_assert(std::is_same_v<int, std::make_signed_t<std::decay_t<Arg>>>,
                "mismatch of an argument type and an integer format specifier");
}

template <typename Arg>
constexpr void validate_hex() {
  static_assert(std::is_same_v<unsigned int, std::decay_t<Arg>>,
                "mismatch of an argument type and a hex format specifier");
}

template <typename Arg>
constexpr void validate_double() {
  static_assert(std::is_same_v<double, std::decay_t<Arg>>,
                "mismatch of an argument type and a floating-point format specifier");
}

template <typename Arg>
constexpr void validate_char() {
  static_assert(std::is_same_v<int, std::decay_t<Arg>>,
                "mismatch of an argument type and a character format specifiers");
}

template <typename Arg>
constexpr void validate_string() {
  static_assert(std::is_convertible_v<char const *, std::decay_t<Arg>>,
                "mismatch of an argument type and a string format specifier");
}

template <typename Arg>
constexpr void validate_pointer() {
  static_assert(std::is_pointer_v<std::decay_t<Arg>>,
                "mismatch of an argument type and a pointer format specifier");
}

template <typename Arg>
constexpr void validate_written() {
  static_assert(std::is_convertible_v<int const *, std::decay_t<Arg>>,
                "mismatch of an argument type and a format specifier %n");
}

template <typename Arg>
constexpr void validate_unknown() {
  static_assert(dependent_false<Arg>{}, "unsupported format specifier");
}

template <auto ...Specs, typename T>
constexpr bool in(T c) {
  return (... || [c](auto spec) { return c == spec; }(Specs));
}

template <typename Fmt, std::size_t N, std::size_t FmtI, typename Arg = void, typename ...Args>
struct validate_helper {
  constexpr bool loop(Fmt &&fmt) const {
    if constexpr (std::get<FmtI>(fmt.array()) == '%') {
      static_assert(std::bool_constant<FmtI + 1 < N>{}, "dangling format specifier");

      constexpr auto const spec_off = specifier_offset<>(fmt);
      static_assert(FmtI + spec_off < N, "no type sub-specifier found");

      constexpr auto const spec = std::get<FmtI + spec_off>(fmt.array());

      if constexpr (spec == '%') {
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Arg, Args...>{}.loop(std::move(fmt));
      } else if constexpr (in<'d', 'i'>(spec)) {
        validate_integer<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (in<'u', 'o', 'x', 'X'>(spec)) {
        validate_hex<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (in<'f', 'F', 'e', 'E', 'g', 'G', 'a', 'A'>(spec)) {
        validate_double<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (spec == 'c') {
        validate_char<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (spec == 's') {
        validate_string<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (spec == 'p') {
        validate_pointer<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else if constexpr (spec == 'n') {
        validate_written<Arg>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args...>{}.loop(std::move(fmt));
      } else {
        validate_unknown<Arg>();
        return false;
      }
    } else
      return validate_helper<Fmt, N, FmtI + 1, Arg, Args...>{}.loop(std::move(fmt));
  }

 private:
  // template <std::size_t I = 1>
  // static constexpr std::size_t
  // length_offset(std::array<typename Fmt::element_type, N> arr) {
  //   return in<'h', 'l', 'j', 'z', 't', 'L'>(std::get<FmtI + I>(arr)) ?
  //       I : length_offset<I + 1>(arr);
  // }

  template <std::size_t I = 1>
  static constexpr std::size_t
  specifier_offset(Fmt const &fmt) {
    if constexpr (FmtI + I == N)
      return N;
    else if constexpr (constexpr auto spec = std::get<FmtI + I>(fmt.array()); in<
                       '%',
                       'd', 'i',
                       'u', 'o', 'x', 'X',
                       'f', 'F', 'e', 'E', 'g', 'G', 'a', 'A',
                       'c',
                       's',
                       'p',
                       'n'
                       >(spec))
      return I;
    else
      return specifier_offset<I + 1>(fmt);
  }
};

template <typename Fmt, std::size_t N, typename Arg, typename ...Args>
struct validate_helper<Fmt, N, N, Arg, Args...> {
  constexpr bool loop(Fmt &&) && {
    static_assert(!sizeof... (Args) && std::is_void_v<Arg>,
                  "too many arguments");
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
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("WARN: arrays aren't supported\n");
  ::printf(fmt, args...);
}

template <typename Fmt, typename ...Args>
requires std::is_pointer_v<std::remove_reference_t<Fmt>> &&
         std::is_convertible_v<char const *, std::decay_t<Fmt>>
constexpr void print [[deprecated("can't validate a dynamic format string")]] (Fmt &&fmt, Args && ...args) {
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
  print("integer is %d\n"_lit, i);

  char const *fmt1 = "integer is %d\n";
  print(fmt1, i);

  char const fmt2[] = "integer is %d\n";
  print(fmt2, i);
  print("integer is %d\n", i);
}
