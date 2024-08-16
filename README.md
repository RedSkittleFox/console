# console
Console is a header only library that provides a simple integration to use integration between console and code nevironment.

```cpp
#include <fox/console.hpp>
#include <iostream>

// Provide global instance
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
```

# Avanced Usage

## Dynamic linking

To use `console` library with dynamic linking you need to expose `console_manager` instance to loader libraries. Check [dynamic linking example](/sample) for more informations. 

## Multithreading

### Builtin

You can enable builtin multithreading support by defining `FOX_CONSOLE_THREAD_SAFE` preprocessor macro. If possible, a critical section approach is recommended. This option is on by default in CMake configuration

### Critical section

Instead of making all the console manager calls thread safe it's recommend to take an advantage of an application wide critical section if possible. Check [critical section](/sample) for more informations.

# Supported compilers

This is a C++ 23 library.

- Microsoft Visual C++ 2022 (msvc) / Build Tools 19.38.33134 (and possibly later)
- Microsoft Visual C++ 2022 (clang) 16.0.5 (and possibly later)

# Integration

`console` is a header only library. You can copy and paste the header into your project or use CMake build system.

## CMake
```cmake
include(FetchContent)

# set(FOX_CONSOLE_THREAD_SAFE OFF) # Disable thread safe
# set(FOX_CONSOLE_ENABLE_EXCEPTIONS OFF) # Disable exceptions

FetchContent_Declare(fox_console GIT_REPOSITORY https://github.com/RedSkittleFox/console.git)
FetchContent_MakeAvailable(fox_console)
target_link_libraries(foo PRIVATE fox::console)
```

# License
This library is licensed under the [MIT License](LICENSE).