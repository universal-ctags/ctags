constexpr int a_int = 1;
constexpr const int b_int = 2;
static constexpr const int c_int = 3;
static constexpr int d_int = 4;
constexpr static const int e_int = 5;
constexpr static int f_int = 6;

constexpr auto g_auto = 7;

constexpr static const char* const h_char_ptr = "this is valid";

static constexpr const char i_char_array[] = "this is also valid";

static constexpr int const& j_ref = 42;

constexpr const int *k_int_ptr = &a_int;

static constexpr const char* const l_raw = R"(setfacl {} -m g:{}:{} {})";

static constexpr const int64_t m_int64 = 120'000;
