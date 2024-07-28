#include <fox/console.hpp>
#include <iostream>

[[nodiscard]] inline fox::console::console_manager& fox::console::console_manager::instance()
{
	static fox::console::console_manager mgr;
	return mgr;
}

fox::console::variable<int> test_var("variable", 1);

std::string to_uppercase(std::string str)
{
	std::ranges::for_each(str, [](char& c) { c = static_cast<char>(std::toupper(c)); });
	return str;
}

fox::console::function<std::string(std::string)> test_func("to_upper", to_uppercase);

int main()
{
	using namespace std::string_view_literals;

	namespace foxc = fox::console;
	auto print_error = [](auto&& error) -> foxc::invoke_result { return foxc::invoke_output{ "Error: " + error.message() }; };

	auto& cm = fox::console::console_manager::instance();

	// Get variable's value
	std::cout
		<< ">> variable\n"
		<< cm.execute("variable")[0].or_else(print_error).value().out
		<< '\n';

	// Set variable's value
	std::cout
		<< ">> variable 5\n"
		<< cm.execute("variable 5")[0].or_else(print_error).value().out << '\n';

	// Invoke function
	std::cout << ">> to_upper \" foxes can fly \"\n" 
		<< cm.execute("to_upper \" foxes can fly \"")[0].or_else(print_error).value().out << '\n';

	return 0;
}