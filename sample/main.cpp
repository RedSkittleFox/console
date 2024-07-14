#include <fox/convar.hpp>
#include <iostream>

[[nodiscard]] inline fox::convar::console_manager& fox::convar::console_manager::instance()
{
	static fox::convar::console_manager mgr;
	return mgr;
}

fox::convar::convar<int> test_var("var", 1);
fox::convar::concommand<int(int)> test_func("func", [](int v) -> int { return v * 2; } );

int main()
{
	using namespace std::string_view_literals;

	std::cout << fox::convar::console_manager::instance().execute("var", {}).value_or("<error>") << '\n';

	std::array params{ "amogus"sv };
	std::cout << fox::convar::console_manager::instance().execute("var", params).value_or("<error>") << '\n';

	std::cout << fox::convar::console_manager::instance().execute("var", {}).value_or("<error>") << '\n';

	std::vector params2{"2"sv};
	std::cout << fox::convar::console_manager::instance().execute("func", params2).value_or("<error>") << '\n';
}