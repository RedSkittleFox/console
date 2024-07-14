#include <gtest/gtest.h>
#include <fox/convar.hpp>

[[nodiscard]] inline fox::convar::console_manager& fox::convar::console_manager::instance()
{
	static fox::convar::console_manager mgr;
	return mgr;
}

TEST(convar_test, console_manager_constructor_destructor)
{
	::fox::convar::console_manager mgr;
	EXPECT_FALSE(std::is_copy_constructible_v<::fox::convar::console_manager>);
	EXPECT_FALSE(std::is_move_constructible_v<::fox::convar::console_manager>);
	EXPECT_FALSE(std::is_copy_assignable_v<::fox::convar::console_manager>);
	EXPECT_FALSE(std::is_move_assignable_v<::fox::convar::console_manager>);
}

using namespace std::string_view_literals;

TEST(convar_test, convar_construct_destroy)
{
	std::vector args{ "1"sv };
	{
		::fox::convar::convar<int> var("convar", -1);
		EXPECT_EQ(::fox::convar::console_manager::instance().execute("convar", {}).value(), "-1");
		EXPECT_EQ(::fox::convar::console_manager::instance().execute("convar", args).value(), "1");
		EXPECT_EQ(::fox::convar::console_manager::instance().execute("convar", {}).value(), "1");
	}

	EXPECT_FALSE(::fox::convar::console_manager::instance().execute("convar", {}).has_value());
	EXPECT_FALSE(::fox::convar::console_manager::instance().execute("convar", args).has_value());
}

TEST(convar_test, convar_properties)
{
	::fox::convar::convar<int> var("convar", -1, "Description", 1);
}