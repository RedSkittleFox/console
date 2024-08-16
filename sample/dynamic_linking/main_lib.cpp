#include <fox/console.hpp>
#include <iostream>

#ifdef _MSC_VER
#define EXPORT __declspec(dllexport)
#define IMPORT __declspec(dllimport)
#elif defined(__GNUC__)
#define EXPORT __attribute__((visibility("default")))
#define IMPORT
#else
#pragma error Unknown dynamic link import/export semantics.
#endif

EXPORT fox::console::console_manager* get_instance_instance()
{
	static fox::console::console_manager mgr;
	return std::addressof(mgr);
}

[[nodiscard]] inline fox::console::console_manager& fox::console::console_manager::instance()
{
	return *get_instance_instance();
}

void echo_function(const std::string& message)
{
	std::cout << message << '\n';
}

fox::console::function<void(const std::string&)> cmd_echo("echo", &echo_function);