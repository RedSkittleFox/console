#include <fox/console.hpp>

#include <iostream>
#include <utility>

#ifdef _MSC_VER
#define EXPORT __declspec(dllexport)
#define IMPORT __declspec(dllimport)
#elif defined(__GNUC__)
#define EXPORT __attribute__((visibility("default")))
#define IMPORT
#else
#pragma error Unknown dynamic link import/export semantics.
#endif

IMPORT fox::console::console_manager* get_instance_instance();

[[nodiscard]] inline fox::console::console_manager& fox::console::console_manager::instance()
{
	return *get_instance_instance();
}

int main()
{
	using namespace std::string_view_literals;
	namespace foxc = fox::console;

	auto print_error = [](auto&& error) -> foxc::invoke_result { return foxc::invoke_output{ "Error: " + error.message(), {} }; };

	auto& cm = fox::console::console_manager::instance();

	std::cout << cm.invoke("echo", {"Foxes can fly"}).or_else(print_error).value().out << '\n';

	return 0;
}