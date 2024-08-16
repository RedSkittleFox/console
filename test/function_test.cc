#include <gtest/gtest.h>
#include <fox/console.hpp>

using namespace std::string_view_literals;
namespace foxc = ::fox::console;

TEST(function, register_unregister)
{
	auto& cm = foxc::console_manager::instance();

	{
		bool invoked = false;
		foxc::function<void(void)> test_func("function", [&]() { invoked = true; });

		EXPECT_EQ(cm.invoke("function", {}).value(), foxc::invoke_output(""));
		EXPECT_TRUE(invoked);
	}

	EXPECT_FALSE(cm.invoke("function", {}).has_value());
}

TEST(function, invoke_zero_args)
{
	auto& cm = foxc::console_manager::instance();

	bool invoked = false;
	foxc::function<void()> test_func("function", [&]() { invoked = true; });

	EXPECT_EQ(cm.invoke("function", {}).value(), foxc::invoke_output(""));
	EXPECT_TRUE(invoked);
}

TEST(function, invoke_one_arg)
{
	auto& cm = foxc::console_manager::instance();

	int v = -1;
	foxc::function<void(int)> test_func("function", [&](int u) { v = u; });

	EXPECT_EQ(cm.invoke("function", {"17"sv}).value(), foxc::invoke_output(""));
	EXPECT_EQ(v, 17);
}

TEST(function, invoke_return_int_void)
{
	auto& cm = foxc::console_manager::instance();

	int v = -1;
	foxc::function<int(void)> test_func("function", [&]() { v = 17; return 17; });

	EXPECT_EQ(cm.invoke("function", {}).value(), foxc::invoke_output("17"));
	EXPECT_EQ(v, 17);
}

TEST(function, invoke_return_int_int)
{
	auto& cm = foxc::console_manager::instance();

	int v = -1;
	foxc::function<int(int)> test_func("function", [&](int u) { v = u; return 17; });

	EXPECT_EQ(cm.invoke("function", {"17"sv}).value(), foxc::invoke_output("17"));
	EXPECT_EQ(v, 17);
}

TEST(function, invoke_invalid_number_of_arguments)
{
	auto& cm = foxc::console_manager::instance();

	bool invoked = false;
	foxc::function<void()> test_func("function", [&]() { invoked = true; });

	EXPECT_FALSE(cm.invoke("function", { "1"sv }).has_value());
	EXPECT_FALSE(invoked);
}

TEST(function, invoke_select_overload_1)
{
	auto& cm = foxc::console_manager::instance();

	[[maybe_unused]] bool func1 = false;
	foxc::function<void()> test_func1("function", [&]() { func1= true; });

	[[maybe_unused]] bool func2 = false;
	foxc::function<void(int)> test_func2("function", [&](int) { func2 = true; });


	EXPECT_TRUE(cm.invoke("function", {}).has_value());
	EXPECT_TRUE(func1);
	EXPECT_FALSE(func2);
}

TEST(function, invoke_select_overload_2)
{
	auto& cm = foxc::console_manager::instance();

	[[maybe_unused]] bool func1 = false;
	foxc::function<void()> test_func1("function", [&]() { func1 = true; });

	[[maybe_unused]] bool func2 = false;
	foxc::function<void(int)> test_func2("function", [&](int) { func2 = true; });

	EXPECT_TRUE(cm.invoke("function", {"1"sv}).has_value());
	EXPECT_FALSE(func1);
	EXPECT_TRUE(func2);
}

TEST(function, define_duplicate)
{
	[[maybe_unused]] bool func1 = false;
	foxc::function<void()> test_func1("function", [&]() { func1 = true; });

	[[maybe_unused]] bool func2 = false;
	EXPECT_ANY_THROW(foxc::function<void()>("function", [&]() { func2 = true; }));
}