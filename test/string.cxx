#include "stuff/string.h"

#include <cassert>
#include <cstring>

template <typename StringT>
void test_string_type() {
    const char* short_str{"Somethin short"};
    const char* long_str{
        "Something long. "
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
        "In nec vehicula turpis. Maecenas tristique mi sit amet "
        "orci massa nunc."};
    const size_t len_short_str = std::strlen(short_str);
    const size_t len_long_str = std::strlen(long_str);

    assert(len_short_str <= StringT::SMALL_STRING_SIZE);

    { // Constructors
        StringT empty{};
        assert(empty == "");
        StringT other_empty("");
        assert(empty == other_empty);
        assert(other_empty == "");

        //  char* constr
        StringT s1{short_str};
        assert(s1 == short_str);
        assert(s1 == StringT{short_str});
        StringT s2{long_str};
        assert(s2 == long_str);
        assert(s2 == StringT{long_str});

        // Copy constr
        StringT s3(s1);
        assert(s1 == s3);
        assert(s3 == short_str);
        StringT s4(s2);
        assert(s2 == s4);
        assert(s4 == long_str);

        // Move constructor
        StringT s5(std::move(s1));
        assert(s5 == s3);
        assert(s3 == short_str);
        StringT s6(std::move(s2));
        assert(s6 == s4);
        assert(s4 == long_str);
    }

    { // Size
        StringT empty{};
        assert(empty.empty() == true);
        assert(empty.length() == 0);

        StringT ss(short_str);
        assert(ss.empty() == false);
        assert(ss.length() == len_short_str);
        assert(ss.size() == len_short_str);

        StringT ls(long_str);
        assert(ls.empty() == false);
        assert(ls.length() == len_long_str);
        assert(ls.size() == len_long_str);
    }

    { // Access and iterator
        size_t i = 0;
        StringT ss{short_str};
        for (auto& c : ss) {
            assert(c == short_str[i]);
            assert(ss[i] == short_str[i]);
            assert(ss.at(i) == short_str[i]);
            i++;
        }
        assert(i == len_short_str);
        i = 0;
        StringT ls{long_str};
        for (const auto c : ls) {
            assert(c == long_str[i]);
            assert(ls[i] == long_str[i]);
            assert(ls.at(i) == long_str[i]);
            i++;
        }
        assert(i == len_long_str);

        // rbegin/rend
        i = len_short_str;
        for (auto it = ss.rbegin(); it != ss.rend(); ++it) {
            i--;
            assert(*it == short_str[i]);
        }
        assert(i == 0);

        i = len_long_str;
        for (auto it = ls.rbegin(); it != ls.rend(); ++it) {
            i--;
            assert(*it == long_str[i]);
        }
        assert(i == 0);

        // Out of range access and null termination
        assert(StringT()[0] == '\0');
        assert(ss[len_short_str] == '\0');
        bool cought = false;
        try {
            char c = ss.at(len_short_str);
            c++; // ignore unsused variable
        } catch (const std::out_of_range& e) { cought = true; }
        assert(cought);

        assert(ls[len_long_str] == '\0');
        cought = false;
        try {
            char c = ls.at(len_long_str);
            c++;
        } catch (const std::out_of_range& e) { cought = true; }
        assert(cought);
    }

    {
        assert(StringT(short_str).starts_with(short_str) == true);
        assert(StringT(short_str).starts_with("S") == true);
        assert(StringT(short_str).starts_with("a") == false);

        assert(StringT(short_str).ends_with(short_str) == true);
        assert(StringT(short_str).ends_with("t") == true);
        assert(StringT(short_str).ends_with("a") == false);

        assert(StringT(long_str).starts_with(long_str) == true);
        assert(StringT(long_str).starts_with("S") == true);
        assert(StringT(long_str).starts_with("a") == false);

        assert(StringT(long_str).ends_with(long_str) == true);
        assert(StringT(long_str).ends_with(".") == true);
        assert(StringT(long_str).ends_with("a") == false);
    }

    {
        // Python-like Replace
        StringT ss{short_str};
        assert(ss.replace("thin", "") == "Some short");
        assert(ss.replace(" ", " not so ") == "Somethin not so short");
        StringT sas{"aaaaaaaaaaaaaaaa"};
        assert(sas.replace("a", "b") == "bbbbbbbbbbbbbbbb");

        StringT ls{long_str};
        assert(
            ls.replace(ls.get_allocator(), "thing", "")
            == "Some long. "
               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
               "In nec vehicula turpis. Maecenas tristique mi sit amet "
               "orci massa nunc.");
        assert(
            ls.replace(" ", " more ", 1)
            == "Something more long. "
               "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
               "In nec vehicula turpis. Maecenas tristique mi sit amet "
               "orci massa nunc.");
        StringT las{"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
        assert(las.replace("a", "b") == "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb");
    }

    { // Join
        StringT s1(short_str);
        auto s5 = StringT{" "}.join("1", "2", "3", "4", s1);
        assert(s5 == "1 2 3 4 Somethin short");

        auto s6 = StringT{" "}.join(
            "1", "2", "3", "4", "something long to trigger alloc", s1);
        assert(s6 == "1 2 3 4 something long to trigger alloc Somethin short");
    }

    { // Comparisson
        StringT a{"Test String A"};
        StringT a2{"Test String A"};
        StringT b{"Test String B"};

        assert(a == a2);
        assert(a != b);
        assert(a <= a2);
        assert(a >= a2);
        assert(a < b);
        assert(b > a);

        assert(a != "Test String B");
        assert(a <= "Test String B");
        assert(a >= "Test String");
        assert(a < "Test String B");
        assert(a > "Test String");

        assert("Test String B" != a);
        assert("Test String B" >= a);
        assert("Test String" <= a);
        assert("Test String B" > a);
        assert("Test String" < a);
    }
}

int main() {
    test_string_type<stuff::String<>>();
    test_string_type<stuff::pmr::String>();
    std::puts("All tests passed successuly");
    return 0;
}
