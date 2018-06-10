#include <array>
#include <cstdio>
#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>

namespace tsprintf {

template <typename T> struct dependent_false: std::false_type {};

template <typename CharT, CharT ...Cs>
struct string_literal {
  static constexpr std::size_t const size = sizeof... (Cs);
  using element_type = CharT;

  constexpr inline auto array() const -> std::array<CharT, sizeof... (Cs)> {
    return {Cs...};
  }
};

template <typename> struct is_string_literal: std::false_type {};
template <typename CharT, CharT ...Cs> struct is_string_literal<string_literal<CharT, Cs...>>: std::true_type {};
template <typename T> inline constexpr bool is_string_literal_v = is_string_literal<T>::value;
template <typename T> using is_string_literal_t                 = typename is_string_literal<T>::type;

// Uses GCC/Clang extension:
// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3599.html
template <typename CharT, CharT ...Cs>
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
constexpr bool contains(T c) {
  return (... || [c](auto spec) { return c == spec; }(Specs));
}

template <typename Fmt, std::size_t N, std::size_t FmtI, typename Args, std::size_t ArgsI>
struct validate_helper {
  constexpr bool loop(Fmt &&fmt, Args &&args) const {
    if constexpr (std::get<FmtI>(fmt.array()) == '%') {
      static_assert(std::bool_constant<FmtI + 1 < N>{}, "dangling format specifier");

      constexpr auto const spec_off = specifier_offset<>(fmt);
      static_assert(FmtI + spec_off < N, "no conversion sub-specifier found");

      constexpr auto const spec = std::get<FmtI + spec_off>(fmt.array());

      if constexpr (spec == '%') {
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (contains<'d', 'i'>(spec)) {
        validate_integer<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (contains<'u', 'o', 'x', 'X'>(spec)) {
        validate_hex<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (contains<'f', 'F', 'e', 'E', 'g', 'G', 'a', 'A'>(spec)) {
        validate_double<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (spec == 'c') {
        validate_char<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (spec == 's') {
        validate_string<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (spec == 'p') {
        validate_pointer<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else if constexpr (spec == 'n') {
        validate_written<decltype(std::get<ArgsI>(args))>();
        return validate_helper<Fmt, N, FmtI + spec_off + 1, Args, ArgsI + 1>{}.loop(
            std::forward<Fmt>(fmt), std::forward<Args>(args));
      } else {
        validate_unknown<decltype(std::get<ArgsI>(args))>();
        return false;
      }
    } else
      return validate_helper<Fmt, N, FmtI + 1, Args, ArgsI>{}.loop(
          std::forward<Fmt>(fmt), std::forward<Args>(args));
  }

 private:
  template <std::size_t I = 1>
  static constexpr std::size_t
  specifier_offset(Fmt const &fmt) {
    if constexpr (FmtI + I == N)
      return N;
    else if constexpr (constexpr auto spec = std::get<FmtI + I>(fmt.array());
                       contains<'%',
                                'd', 'i',
                                'u', 'o', 'x', 'X',
                                'f', 'F', 'e', 'E', 'g', 'G', 'a', 'A',
                                'c',
                                's',
                                'p',
                                'n'>(spec))
      return I;
    else
      return specifier_offset<I + 1>(fmt);
  }
};

template <typename Fmt, std::size_t N, typename Args, std::size_t ArgsI>
struct validate_helper<Fmt, N, N, Args, ArgsI> {
  constexpr bool loop(Fmt &&, Args &&args) && {
    static_assert(ArgsI == std::tuple_size_v<std::remove_reference_t<Args>>,
                  "extra arguments");
    return true;
  }
};

template <typename Fmt, std::size_t N, typename Args>
constexpr bool validate(Fmt &&fmt, Args &&args) {
  return validate_helper<Fmt, N, 0, Args, 0>{}.loop(
      std::forward<Fmt>(fmt), std::forward<Args>(args));
}

template <typename Fmt, typename Args>
constexpr bool validate_static(Fmt &&fmt, Args &&args) {
  constexpr std::size_t const N = std::remove_reference_t<Fmt>::size;
  return validate<Fmt, N, Args>(std::forward<Fmt>(fmt), std::forward<Args>(args));
}

}

template <typename Fmt, typename ...Args>
constexpr void tsprint(Fmt &&fmt, Args && ...args);

template <typename Fmt, typename ...Args>
requires std::is_array_v<std::remove_reference_t<Fmt>>
constexpr void tsprint [[deprecated("arrays aren't supported")]] (Fmt &&fmt, Args && ...args) {
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("WARN: arrays aren't supported\n");
  ::printf(fmt, args...);
}

template <typename Fmt, typename ...Args>
requires std::is_pointer_v<std::remove_reference_t<Fmt>> &&
         std::is_convertible_v<char const *, std::decay_t<Fmt>>
constexpr void tsprint [[deprecated("can't validate a dynamic format string")]] (Fmt &&fmt, Args && ...args) {
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("WARN: can't validate a dynamic format string\n");
  ::printf(fmt, args...);
}

template <typename Fmt, typename ...Args>
requires is_string_literal_v<Fmt>
constexpr void tsprint(Fmt &&fmt, Args && ...args) {
  details::validate_static(fmt, std::make_tuple(std::forward<Args>(args)...));
  ::printf("INFO: ======> %s\n", __PRETTY_FUNCTION__);
  ::printf("INFO: the format string staticly verified\n");
  ::printf(fmt.array().data(), args...);
}

namespace printf_literal {

template <typename CharT, CharT ...Cs>
struct printf_op {
  template <typename ...Args>
  void operator () (Args && ...args) {
    tsprint(string_literal<CharT, Cs..., 0>{}, std::forward<Args>(args)...);
  }
};

template <typename CharT, CharT ...Cs>
constexpr printf_op<CharT, Cs...> operator ""_printf() {
  return {};
}

}

}
