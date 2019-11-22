//
// stuff library - A library of small C++ utilities
// author: Xavier Thomas
// 
// You should have received a copy of the MIT License
// along with this software. If not, see <https://opensource.org/licenses/MIT>.
//
// Immutable string Python style
// TODO: better description
//
// Benefits:
// - Performant short strings (SBO)
// - Implicit sharing ('free' copy)
// - Safe for concurent use (imutability, shared_ptr)
// - Allocator support

// TODO:
// - make template parameter for value type (char uchar8_t)
// - make template parameter for short string cut-off value
// - clang-format
// - clang-tidy
// - micro-benches
// - use case benches


#pragma once

//#include <format>
//#include <compare>
#include <atomic>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <limits>
#include <memory>
#include <memory_resource>
#include <ostream>
#include <utility>

namespace stuff {

namespace detail {

struct SharedStr {
    // Note: Could put allocator in here, maybe size too
    std::atomic<std::size_t> ref_count_ = 0;
    char data_[1]; // first member for alignment
    
    char* data() { return &data_[0]; }
};

struct alignas(alignof(SharedStr)) SAlloc {};

template <typename Allocator>
[[nodiscard]] SharedStr* allocate_shared_str_default_init(
        Allocator& alloc, std::size_t length) {
    using alloc_traits = typename std::allocator_traits<Allocator>;

    using Asa = typename alloc_traits::template rebind_alloc<SAlloc>;
    using Asa_traits = typename alloc_traits::template rebind_traits<SAlloc>;
    using pointer = typename Asa_traits::pointer; 
    Asa a2(alloc);
    // We need length + 1 but SharedStr already comes with one
    std::size_t n = sizeof(SharedStr) + length;
    pointer p = Asa_traits::allocate(a2, 
        (n + sizeof(SAlloc) - 1) / sizeof(SAlloc));
    assert(reinterpret_cast<std::uintptr_t>(p) % alignof(SharedStr) == 0 &&
         "allocator does not respect alignment");

    // Init lifetime and set ref_count_ to 0
    // SharedStr* result = new(p) SharedStr();
    using Ass = typename alloc_traits::template rebind_alloc<SharedStr>;
    using Ass_traits = 
        typename alloc_traits::template rebind_traits<SharedStr>;
    Ass a3(alloc);
    SharedStr* result = reinterpret_cast<SharedStr*>(p);
    Ass_traits::construct(a3, result);

    // Init lifetime for the rest of the elements to avoid 'technical UB' 
    // but should not generate any code
    // No way do to default init using allocator(traits) API
    for(std::size_t i = 1; i < length + 1; i++) { 
        // alloc_traits::construct(alloc, &result->data()[i]);  // value init
        ::new(&result->data()[i]) char; // default init. Note: not 'char()'
    }
    return result;
}

template <typename Allocator>
void deallocate_shared_str(
        Allocator& alloc, SharedStr* ss, std::size_t length) {
    using alloc_traits = std::allocator_traits<Allocator>;

    for(std::size_t i = 1; i < length + 1; i++) { 
        alloc_traits::destroy(alloc , &ss->data()[i]);
    }

    // ss->~SharedStr();
    using Ass = typename alloc_traits::template rebind_alloc<SharedStr>;
    using Ass_traits = 
        typename alloc_traits::template rebind_traits<SharedStr>;
    Ass a3(alloc);
    Ass_traits::destroy(a3, ss);
    
    using Asa = typename alloc_traits::template rebind_alloc<SAlloc>;
    using Asa_traits = typename alloc_traits::template rebind_traits<SAlloc>;
    Asa a2(alloc);
    std::size_t n = sizeof(SharedStr) + length;
    Asa_traits::deallocate(a2, reinterpret_cast<SAlloc*>(ss), 
        (n + sizeof(SAlloc) - 1) / sizeof(SAlloc));
}

struct LongStr {
    static constexpr std::size_t SIZE_BITS = 8 * sizeof(std::size_t) - 1;
    static constexpr std::size_t MAX_SIZE = (1ul << SIZE_BITS) - 1;

    bool is_long_ : 1;
    std::size_t size_ : SIZE_BITS;
    SharedStr* ssp_;

    LongStr() = delete;

    template<typename Allocator>
    LongStr(Allocator& alloc, std::size_t size) {
        assert(size <= MAX_SIZE);
        is_long_ = true;
        size_ = (size & MAX_SIZE);
        ssp_ = allocate_shared_str_default_init(alloc, size);
        increment_ref_count();
    }
    
    LongStr(const LongStr& other) noexcept
        : is_long_(true), size_(other.size_), ssp_(other.ssp_) {
        increment_ref_count(); 
    }
    LongStr(LongStr&& other) noexcept 
        : is_long_(true), size_(other.size_), ssp_(other.ssp_) {
        other.ssp_ = nullptr; 
    }

    // Note: NOT an RAII/safe type. decrement_ref_count() needs to be called
    // to avoid memory leaks
    ~LongStr() = default;

    void increment_ref_count() { if (ssp_ != nullptr) ++(ssp_->ref_count_); }

    template<typename Allocator>
    void decrement_ref_count(Allocator& alloc) {
        if (ssp_ != nullptr && --(ssp_->ref_count_) == 0) { 
            deallocate_shared_str(alloc, ssp_, size_);
            ssp_ = nullptr;
        }
    }
};

struct ShortStr {
    static constexpr std::size_t SIZE_BITS = 8 * sizeof(std::uint8_t) - 1;
    static constexpr std::uint8_t MAX_SIZE = (1ul << SIZE_BITS) - 1;
    static constexpr std::size_t BUFFER_SIZE = sizeof(LongStr) - 1;
    static_assert(BUFFER_SIZE <= MAX_SIZE);

    bool is_long_ : 1;
    std::uint8_t size_: SIZE_BITS;
    std::array<char, BUFFER_SIZE> data_;

    ShortStr() noexcept : ShortStr(0) { data_[0] = '\0'; }

    ShortStr(std::uint8_t size) noexcept {
        assert(size <= MAX_SIZE);
        is_long_ = false;
        size_ = (size & MAX_SIZE);
    }
};

static_assert(sizeof(ShortStr) == sizeof(LongStr));

template <typename Allocator = std::allocator<char>>
struct UnionStr {
    [[no_unique_address]] Allocator allocator_;
    union {
        LongStr long_;
        ShortStr short_;
    };

    UnionStr() noexcept : short_()  {}
    UnionStr(const Allocator& alloc) noexcept : allocator_(alloc), short_() {}

    UnionStr(const UnionStr& other) noexcept : allocator_(other.allocator_) {
        if (other.is_long()) { 
            new(&long_) LongStr(other.long_); 
        } else { 
            new(&short_) ShortStr(other.short_); 
        }
    }

    UnionStr(UnionStr&& other) noexcept 
        : allocator_(std::move(other.allocator_)) {
        if (other.is_long()) { 
            new(&long_) LongStr(std::move(other.long_)); 
        } else { 
            new(&short_) ShortStr(other.short_); 
        }
    }

    UnionStr(const Allocator& alloc, std::size_t size) : allocator_(alloc) {
        if (size >= ShortStr::BUFFER_SIZE) {
            new(&long_) LongStr(allocator_, size);
        } else {
            new(&short_) ShortStr(static_cast<uint8_t>(size));
        }
    }

    ~UnionStr() {
        if (is_long()) { 
            long_.decrement_ref_count<Allocator>(allocator_);
            long_.~LongStr(); 
        } else { 
            short_.~ShortStr(); 
        }
    }

    UnionStr& operator=(UnionStr& other) = delete;
    UnionStr& operator=(UnionStr&& other) = delete;
    
    [[nodiscard]] Allocator 
    get_allocator() const noexcept { return allocator_; }

    [[nodiscard]] bool is_long() const noexcept { return long_.is_long_ ; }
    
    [[nodiscard]] std::size_t length() const noexcept {
        return is_long() ? long_.size_ : short_.size_;
    }
    
    [[nodiscard]] char* data() noexcept {
        return is_long() ? long_.ssp_->data() : short_.data_.data();
    }

    [[nodiscard]] const char* data() const noexcept {
        return is_long() ? long_.ssp_->data() : short_.data_.data();
    }

};

} // namespace detail


template <typename Allocator = std::allocator<char>>
class String {
public:
    // types
    using value_type = char;
    using pointer = char*;
    using const_pointer = const char*;
    using reference = char&;
    using const_reference = const char&;
    using const_iterator = const char*;
    using iterator = const_iterator;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using allocator_type = Allocator;
    
    static_assert(
        std::is_same_v<value_type, typename allocator_type::value_type>,
        "Invalid Allocator::value_type");

    static_assert(std::is_standard_layout<detail::LongStr>::value);
    static_assert(std::is_standard_layout<detail::ShortStr>::value);
    static_assert(std::is_standard_layout<detail::UnionStr<Allocator>>::value);
    
    // Maximum size of a string that still fits the small buffer and 
    // thus for which allocation is optimized away
    static constexpr std::size_t SMALL_STRING_SIZE 
        = detail::ShortStr::BUFFER_SIZE - 1;
    static constexpr std::size_t npos = std::string_view::npos;

private:
    using alloc_traits = std::allocator_traits<allocator_type>;

public:
    String() noexcept(noexcept(Allocator())) : String(Allocator()) {}
    explicit String(const Allocator& alloc) noexcept : str_(alloc) {}

    String(const char* str, const Allocator& alloc = Allocator())
        : str_(alloc, std::strlen(str)) {
        std::memcpy(str_.data(), str, str_.length() + 1);
    }

    String(const char* str, std::size_t count, 
            const Allocator& alloc = Allocator())
        : str_(alloc, count) {
        std::memcpy(str_.data(), str, count);
        str_.data()[count] = '\0';
    }

    // Do not "select" allocator to see if it propagates on copy.
    // Since we do not duplicate the allocated container area (the 
    // char[] inside the shared_ptr) but share it, it seems logical 
    // to always propagate the allocator.
    String(const String& other) noexcept : str_(other.str_) {}

    // Note: we could avoid copy if (alloc == other.allocator_)
    String(const String& other, const Allocator& alloc) noexcept
        : str_(alloc, other.length()) {
        std::memcpy(str_.data(), other.data(), other.length() + 1);
    }

    String(String&& other) = default;

    // Note: we could avoid copy if (alloc == other.allocator_)
    String(String&& other, const Allocator& alloc) noexcept
        : str_(alloc, other.length()) {
        std::memcpy(str_.data(), other.data(), other.length() + 1);
    }

    String& operator=(String& other) = delete;
    String& operator=(String&& other) = delete;

    [[nodiscard]] Allocator 
    get_allocator() const noexcept { return str_.get_allocator(); }

    // Iterators

    [[nodiscard]] const_iterator begin() const noexcept { return cbegin(); }
    [[nodiscard]] const_iterator cbegin() const noexcept { return data(); }
    [[nodiscard]] const_iterator end() const noexcept { return cend(); }
    
    [[nodiscard]] const_iterator 
    cend() const noexcept { return &data()[length()]; }

    [[nodiscard]] const_reverse_iterator rbegin() const noexcept {
        return crbegin();
    }

    [[nodiscard]] const_reverse_iterator crbegin() const noexcept {
        return const_reverse_iterator(&data()[length()]);
    }

    [[nodiscard]] const_reverse_iterator rend() const noexcept {
        return crend();
    }

    [[nodiscard]] const_reverse_iterator crend() const noexcept {
        return const_reverse_iterator(data());
    }

    // Capacity
    
    [[nodiscard]] std::size_t length() const noexcept { return str_.length(); }
    [[nodiscard]] std::size_t size() const noexcept { return length(); }
    [[nodiscard]] bool empty() const noexcept { return length() == 0; }

    // Access
    
    [[nodiscard]] const char& 
    operator[](std::size_t pos) const { return data()[pos]; }

    [[nodiscard]] const char& at(std::size_t pos) const {
        check_bounds(pos);
        return data()[pos];
    }

    [[nodiscard]] constexpr const_reference front() const { return data()[0]; }
    [[nodiscard]] constexpr 
        const_reference back() const { return data()[length() - 1]; }

    [[nodiscard]] const char* c_str() const { return data(); }
    [[nodiscard]] const char* data() const { return str_.data(); }

    // Comparisson

    [[nodiscard]] int compare(const String& other) const noexcept {
        return compare(static_cast<std::string_view>(other));
    }

    [[nodiscard]] int compare(const std::string_view str) const noexcept {
        return static_cast<std::string_view>(this).compare(str);
    }

//    auto operator <=>(const String& other) const {
//        return <=>(static_cast<std::string_view>(other));
//    }
//
//    auto operator <=>(const std::string_view str) const {
//        auto comp = this.compare(str):
//        if (comp < 0 ) { return std::strong_ordering::less }
//        else if (comp == 0) { return std::strong_oredering::equal }
//        else { return std::strong_ordering::greater }
//    }

    // Implicit conversion
    operator std::string_view() const noexcept { 
        return std::string_view(data(), size()); 
    }

    // Operations
    
    [[nodiscard]] bool starts_with(const std::string_view sv) const noexcept {
        return static_cast<std::string_view>(*this).starts_with(sv);
    }
    [[nodiscard]] bool starts_with(char c) const noexcept {
        return static_cast<std::string_view>(*this).starts_with(c);
    }
    [[nodiscard]] bool starts_with(const char* s) const {
        return starts_with(std::string_view(s));
    }

    [[nodiscard]] bool ends_with(const std::string_view sv) const noexcept {
        return static_cast<std::string_view>(*this).ends_with(sv);
    }
    [[nodiscard]] bool ends_with(char c) const noexcept {
        return static_cast<std::string_view>(*this).ends_with(c);
    }
    [[nodiscard]] bool ends_with(const char* s) const {
        return ends_with(std::string_view(s));
    }

    [[nodiscard]] bool contains(const std::string_view sv) const noexcept {
        return static_cast<std::string_view>(*this).find(sv) != npos;
    }
    [[nodiscard]] bool contains(char c) const noexcept {
        return static_cast<std::string_view>(*this).find(c) != npos;
    }
    [[nodiscard]] bool contains(const char* s) const {
        return contains(std::string_view(s));
    }

    [[nodiscard]] std::string_view substr_view(
            std::size_t pos = 0, std::size_t count = npos) const noexcept {
        return static_cast<std::string_view>(*this).substr(pos, count);
    }

    // NOTE: We do like std::string and use a default constructed allocator
    // instead of copying this->allocator_
    [[nodiscard]] String 
    substr(std::size_t pos = 0, std::size_t count = npos) const {
        return substr(Allocator(), pos, count);
    }
    
    [[nodiscard]] String 
    substr(const Allocator& alloc, 
            std::size_t pos = 0, std::size_t count = npos) const {
        const auto sv = substr_view(pos, count);
        return String(sv.data(), sv.length(), alloc);
    }
    
    [[nodiscard]] std::size_t 
    find(std::string_view sv, std::size_t pos = 0) const noexcept {
        return static_cast<std::string_view>(*this).find(sv, pos);
    }

    // TODO more find() rfind() ...

    // TODO replace() std::string style but const and returning a new string
    
    // Python-like replace

    // NOTE: Default constructed alloc, NOT a copy of this->allocator_
    [[nodiscard]] String replace(
            std::string_view old, std::string_view n, 
            std::size_t count 
                = std::numeric_limits<std::size_t>::max()) const {
        return replace(Allocator(), old, n, count);
    }

    [[nodiscard]] String replace(
            const Allocator alloc, 
            std::string_view old, std::string_view n, 
            std::size_t count 
                = std::numeric_limits<std::size_t>::max() ) const {
        std::size_t pos = 0;
        std::size_t c = 0;
        while(c < count) {
            pos = find(old, pos);
            if (pos == npos) { break; }
            c++;
            pos = pos + old.length();
        }
        std::ptrdiff_t diff = static_cast<std::ptrdiff_t>(old.length()) 
            - static_cast<std::ptrdiff_t>(n.length());
        auto len = static_cast<std::ptrdiff_t>(length()) 
            - static_cast<std::ptrdiff_t>(c) * diff;

        String out(static_cast<std::size_t>(len), alloc);
        char* it_out = out.str_.data();
        c = 0;
        pos = 0;
        std::size_t found_pos = 0;
        while(c < count) {
            found_pos = find(old, pos);
            if (found_pos == npos) { break; }
            std::size_t l = found_pos - pos;
            std::memcpy(it_out, &data()[pos], l);
            it_out += l;
            std::memcpy(it_out, n.data(), n.length());
            it_out += n.length();
            pos = found_pos + old.length();
            c++;
        }
        std::size_t l = length() - pos;
        std::memcpy(it_out, &data()[pos], l);
        it_out += l;
        it_out[0] = '\0'; 
        return out;
    }

    // Python-like join

    // NOTE: Default constructed alloc, NOT a copy of this->allocator_
    template <typename... Args>
        // requires (... && std::is_convertible_v<Args, std::string_view>)
    [[nodiscard]] String join(const Args&... args) const {
        return join(Allocator(), args...);
    }

    template <typename... Args>
        // requires (... && std::is_convertible_v<Args, std::string_view>)
    [[nodiscard]] String 
    join(const Allocator& alloc, const Args&... args) const {
        constexpr size_t n_args = sizeof...(Args);
        std::array<std::string_view, n_args> args_array{{args...}};
        return join(alloc, args_array.cbegin(), args_array.cend());
    }

private:
    // TODO make const if possible
    // join()/replace() make this difficult to do 
    detail::UnionStr<Allocator> str_;

    // Alocate if necessary and set size but requires data to be filled later
    String(const std::size_t size, const Allocator& alloc = Allocator())
        : str_(alloc, size) {}

    void check_bounds(size_t index) const {
        if (index >= this->size()) {
            // const auto what = std::format(
            //     "String::check_bounds: "
            //     "index (which is {}) "
            //     ">= this->size() (which is {})",
            //     index,
            //     this->size());
            std::array<char, 128> what;
            std::snprintf(what.data(), 128, 
                "String::check_bounds: "
                "index (which is %lu) "
                ">= this->size() (which is %lu)",
                index,
                this->size());
            throw std::out_of_range(what.data());
        }
    }

    template <typename Iter>
        // requires std::is_same_v<
        //     std::iterator_traits<Iter>::value_type,
        //     std::string_view>
    [[nodiscard]] String 
    join(const Allocator& alloc, Iter first, Iter last) const {
        std::size_t len = 0;
        std::size_t n = 0;
        for (auto it = first; it != last; it++) {
            len += it->length();
            n++;
        }
        len += (n - 1) * length();

        String ret(len, alloc);
        auto it = first;
        char* dest = ret.str_.data();
        std::memcpy(dest, it->data(), it->length());
        dest += it->length();
        it++;
        for (; it != last; it++) {
            std::memcpy(dest, data(), length());
            dest += length();
            std::memcpy(dest, it->data(), it->length());
            dest += it->length();
        }
        dest[0] = '\0';
        // Do not use std::move, let the compiler do named RVO .
        return ret;
    }

};

// Non-member functions

// Comparisson 
template <typename Allocator>
bool operator ==(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        == static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator !=(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        != static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator <(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        < static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator <=(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        <= static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator >(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        > static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator >=(const String<Allocator>& lhs, const String<Allocator>& rhs) {
    return static_cast<std::string_view>(lhs) 
        >= static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator ==(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) == rhs;
}

template <typename Allocator>
bool operator !=(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) != rhs;
}

template <typename Allocator>
bool operator <(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) < rhs;
}

template <typename Allocator>
bool operator <=(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) <= rhs;
}

template <typename Allocator>
bool operator >(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) > rhs;
}

template <typename Allocator>
bool operator >=(const String<Allocator>& lhs, const std::string_view& rhs) {
    return static_cast<std::string_view>(lhs) >= rhs;
}

template <typename Allocator>
bool operator ==(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs == static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator !=(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs != static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator <(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs < static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator <=(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs <= static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator >(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs > static_cast<std::string_view>(rhs);
}

template <typename Allocator>
bool operator >=(const std::string_view& lhs, const String<Allocator>& rhs) {
    return lhs >= static_cast<std::string_view>(rhs);
}

// iostream support
template <typename Allocator>
std::ostream& operator<<(std::ostream& os, const String<Allocator>& s) {
    os << static_cast<std::string_view>(s);
    return os;
}

namespace pmr {
    using String = ::stuff::String<std::pmr::polymorphic_allocator<char>>;
} //namespace pmr

} // namespace stuff
