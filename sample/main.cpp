#include <fox/console.hpp>
#include <iostream>

[[nodiscard]] inline fox::console::console_manager& fox::console::console_manager::instance()
{
	static fox::console::console_manager mgr;
	return mgr;
}

fox::console::variable<int> test_var("var", 1);
fox::console::function<int(int)> test_func("func", [](int v) -> int { return v * 2; } );

int main()
{
	using namespace std::string_view_literals;

	namespace foxc = fox::console;
	auto print_error = [](auto&& error) -> foxc::invoke_result { return foxc::invoke_output{ "Error: " + error.message() }; };

	auto& cm = fox::console::console_manager::instance();

	std::cout << cm.invoke("var", {}).or_else(print_error).value().out << '\n';

	std::array params{ "amogus"sv };
	std::cout << cm.invoke("var", params).or_else(print_error).value().out << '\n';

	std::cout << cm.invoke("var", {}).or_else(print_error).value().out << '\n';

	std::vector params2{"2"sv};
	std::cout << cm.invoke("func", params2).or_else(print_error).value().out << '\n';
}