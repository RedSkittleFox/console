#include <memory>
#include <ranges>

#include <gtest/gtest.h>
#include <fox/console.hpp>

namespace foxc = ::fox::console;
using namespace std::string_view_literals;

[[nodiscard]] fox::console::console_manager& fox::console::console_manager::instance()
{
	static fox::console::console_manager mgr;
	return mgr;
}

TEST(console_manager, constructor_destructor)
{
	::fox::console::console_manager mgr;
	EXPECT_FALSE(std::is_copy_constructible_v<::fox::console::console_manager>);
	EXPECT_FALSE(std::is_move_constructible_v<::fox::console::console_manager>);
	EXPECT_FALSE(std::is_copy_assignable_v<::fox::console::console_manager>);
	EXPECT_FALSE(std::is_move_assignable_v<::fox::console::console_manager>);
}

struct execute_test_param
{
	std::string_view cmd_string;
	std::vector<std::string_view> cmds;

	execute_test_param(std::string_view cmd, std::vector<std::string_view> cmds)
		: cmd_string(cmd), cmds(cmds) {}
};

class execute : public ::testing::TestWithParam<execute_test_param>
{
public:
	std::unique_ptr<foxc::variable<int>> c_var_0;
	std::unique_ptr<foxc::variable<int>> c_var_1;
	std::unique_ptr<foxc::function<void()>> c_func_0;
	std::unique_ptr<foxc::function<void(int)>> c_func_1;
	std::unique_ptr<foxc::function<int()>> c_func_2_a;
	std::unique_ptr<foxc::function<int(int)>> c_func_2_b;

	std::unique_ptr<foxc::function<std::string()>> c_func_3_a;
	std::unique_ptr<foxc::function<std::string(std::string)>> c_func_3_b;
	std::unique_ptr<foxc::function<std::string(std::string, std::string)>> c_func_3_c;
	std::unique_ptr<foxc::function<std::string(std::string, std::string, std::string)>> c_func_3_d;

protected:
	void SetUp() override
	{
		c_var_0 = std::make_unique<foxc::variable<int>>("var0", -1, "var0 description");
		c_var_1 = std::make_unique<foxc::variable<int>>("var1", -17, "var1 description", 0, true);
		c_func_0 = std::make_unique<foxc::function<void()>>("func0", []() {}, "func0 description");
		c_func_1 = std::make_unique<foxc::function<void(int)>>("func1", [](int) {}, "func1 description", 0, true);
		c_func_2_a = std::make_unique<foxc::function<int()>>("func2", []() -> int { return 17; }, "func2a description");
		c_func_2_b = std::make_unique<foxc::function<int(int)>>("func2", [](int v) -> int { return v; }, "func2b description");

		c_func_3_a = std::make_unique<foxc::function<std::string()>>("func3",
			[]() -> std::string { return ""; },
			"func3a description");
		c_func_3_b = std::make_unique<foxc::function<std::string(std::string)>>("func3",
			[](std::string a) -> std::string { return a; },
			"func3b description");
		c_func_3_c = std::make_unique<foxc::function<std::string(std::string, std::string)>>("func3",
			[](std::string a, std::string b) -> std::string { return a + b; },
			"func3c description");
		c_func_3_d = std::make_unique<foxc::function<std::string(std::string, std::string, std::string)>>("func3",
			[](std::string a, std::string b, std::string c) -> std::string { return a + b + c; },
			"func3d description");
	}

	void TearDown() override
	{
		c_var_0.reset();
		c_var_1.reset();
		c_func_0.reset();
		c_func_1.reset();
		c_func_2_a.reset();
		c_func_2_b.reset();
	}
};

TEST_P(execute, execute_command)
{
	auto& mgr = foxc::console_manager::instance();

	auto cmd_string = GetParam().cmd_string;
	auto cmds = GetParam().cmds;

	const auto r = mgr.execute(cmd_string);
	ASSERT_EQ(std::size(r), std::size(cmds));

	for (std::size_t i = {}; i < std::size(r); ++i)
	{
		auto&& expected = cmds[i];
		auto&& actual = r[i];
		EXPECT_TRUE(actual.has_value());
		if(actual.has_value())
		{
			EXPECT_EQ(expected, actual.value().out);
		}
	}
}

static std::string execute_test_names(const testing::TestParamInfo<execute::ParamType>& info)
{
	auto str = std::to_string(info.index) + ":" + static_cast<std::string>(info.param.cmd_string);
	for(auto& c : str)
	{
		if (!std::isalnum(c))
			c = '_';
	}

	return str;
}

INSTANTIATE_TEST_SUITE_P(args, execute,
	testing::Values(
		execute_test_param("func3", { "" }),
		execute_test_param("func3;", { "" }),
		execute_test_param("func3 a", { "a" }),
		execute_test_param("func3   a", { "a" }),
		execute_test_param("func3   a   ", { "a" }),
		execute_test_param("func3   a;", { "a" }),
		execute_test_param("func3   a  ;", { "a" }),
		execute_test_param("func3   a  ;  ", { "a" }),
		execute_test_param("func3   a;  ", { "a" }),
		execute_test_param("func3   a;  func3 b", { "a", "b" }),
		execute_test_param("func3   a;  func3 b;", { "a", "b" }),
		execute_test_param("func3   a;func3 b ;", { "a", "b" }),
		execute_test_param("func3   a;func3 b ; ", { "a", "b" }),
		execute_test_param("func3   a;func3 b ; ", { "a", "b" }),
		execute_test_param("func3   a; func3 b; ", { "a", "b" }),
		execute_test_param("func3 a; func3 b; ", { "a", "b" }),
		execute_test_param("func3 a; func3; ", { "a", "" }),
		execute_test_param("func3 a; func3;", { "a", "" }),
		execute_test_param("func3; func3 b;", { "", "b" }),
		execute_test_param("func3 \"a b\"; func3 b;", { "a b", "b" }),
		execute_test_param("func3 \"a\\\"b\"; func3 b;", { "a\"b", "b" }),
		execute_test_param("func3 \" \"; func3 b;", { " ", "b" }),
		execute_test_param("func3 \" a \"; func3 b;", { " a ", "b" }),
		execute_test_param("func3 \" a \";", { " a "}),
		execute_test_param("func3 \" a \"", { " a " }),
		execute_test_param("func3 \" a \" ", { " a " }),
		execute_test_param("func3 \" a \" c ", { " a c" }),
		execute_test_param("func3 b; func3\" a \" c ", { "b", " a c" }),
		execute_test_param("func3 b; func3\" a \" c ;", { "b", " a c" }),
		execute_test_param("func3 b; func3\" a; \" c ;", { "b", " a; c" })
		),
	execute_test_names
);