#include <list>
#include <gtest/gtest.h>
#include <fox/console.hpp>

using namespace std::string_view_literals;
namespace foxc = ::fox::console;

TEST(variable, register_unregister)
{
	auto& cm = foxc::console_manager::instance();

	{
		foxc::variable<int> var("variable", -1);
		EXPECT_EQ(cm.invoke("variable", {}).value(), foxc::invoke_output("-1"));
		EXPECT_EQ(cm.invoke("variable", {"1"sv}).value(), foxc::invoke_output("1"));
		EXPECT_EQ(cm.invoke("variable", {}).value(), foxc::invoke_output("1"));
	}

	EXPECT_FALSE(cm.invoke("variable", {}).has_value());
	EXPECT_FALSE(cm.invoke("variable", {"1"sv}).has_value());
}

TEST(variable, properties)
{
	foxc::variable<int> var("variable", -1, "Foxes can fly", 1);

	EXPECT_EQ(var.name(), "variable");
	EXPECT_EQ(var.description(), "Foxes can fly");
	EXPECT_EQ(var.get(), -1);
	EXPECT_EQ(var.permission_level(), 1);
}

TEST(variable, invoke_setter_template)
{
	auto& cm = foxc::console_manager::instance();

	foxc::variable<int> var("variable", -1, "Foxes can fly", 1);
	EXPECT_EQ(var.get(), -1);
	{
		auto result = cm.invoke("variable", std::list{ "17"sv });
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("17"));
	}
	EXPECT_EQ(var.get(), 17);
}

TEST(variable, invoke_setter_span)
{
	auto& cm = foxc::console_manager::instance();

	foxc::variable<int> var("variable", -1, "Foxes can fly", 1);
	EXPECT_EQ(var.get(), -1);

	{
		std::array args{ "17"sv };
		auto result = cm.invoke("variable", args);
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("17"));
	}

	EXPECT_EQ(var.get(), 17);
}

TEST(variable, invoke_getter)
{
	auto& cm = foxc::console_manager::instance();

	foxc::variable<int> var("variable", -1, "Foxes can fly", 1);

	{
		EXPECT_EQ(var.get(), -1);
		auto result = cm.invoke("variable", {});
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("-1"));
	}

	EXPECT_EQ(cm.invoke("variable", { "17"sv }).value(), foxc::invoke_output("17"));

	{
		EXPECT_EQ(var.get(), 17);
		auto result = cm.invoke("variable", {});
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("17"));
	}
}

TEST(variable, define_duplicate)
{
	foxc::variable<int> var("variable");

	EXPECT_ANY_THROW(foxc::variable<int>("variable"));
}

namespace 
{
	struct test_type_1
	{
		int v;

		auto operator<=>(const test_type_1& rhs) const = default;
	};

	struct test_type_2
	{
		
	};
}

namespace fox::console
{
	template<>
	struct parser<test_type_1>
	{
		[[nodiscard]] std::optional<invoke_output> operator()(const test_type_1& v) const
		{
			std::ostringstream ss;
			ss << "test_type_1: " << v.v;
			return invoke_output{ ss.str(), {} };
		}

		[[nodiscard]] std::optional<test_type_1> operator()(std::string_view sv) const
		{
			test_type_1 v;
			try
			{
				std::istringstream ss(static_cast<std::string>(sv));
				ss >> v.v;
				return v;
			}
			catch (...)
			{
				return std::nullopt;
			}
		}
	};
}

TEST(variable, custom_parser)
{
	auto& cm = foxc::console_manager::instance();

	foxc::variable<test_type_1> var("variable", { -1 }, "Foxes can fly", 1);

	{
		EXPECT_EQ(var.get(), test_type_1{ -1 });
		auto result = cm.invoke("variable", {});
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("test_type_1: -1"));
	}

	EXPECT_TRUE(cm.invoke("variable", { "17"sv }).has_value());

	{
		EXPECT_EQ(var.get(), test_type_1{ 17 });
		auto result = cm.invoke("variable", {});
		ASSERT_TRUE(result.has_value());
		EXPECT_EQ(result.value(), foxc::invoke_output("test_type_1: 17"));
	}
}

TEST(variable, invalid_custom_parser)
{
	EXPECT_FALSE(fox::console::parsable<test_type_2>);
}