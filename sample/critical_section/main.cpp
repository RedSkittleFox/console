#undef FOX_CONSOLE_THREAD_SAFE
#include <fox/console.hpp>

#include <concepts>
#include <thread>
#include <iostream>
#include <mutex>
#include <barrier>

[[nodiscard]] inline fox::console::console_manager& fox::console::console_manager::instance()
{
	static fox::console::console_manager mgr;
	return mgr;
}

struct
{
	std::mutex mutex;
	bool should_enter_critical_section = false;
	std::barrier<> barrier_enter{ 2 };
	std::barrier<> barrier_leave{ 2 };

	void trigger(std::invocable auto&& Func)
	{
		{
			std::unique_lock<std::mutex> lck(mutex);
			should_enter_critical_section = true;
		}
		
		barrier_enter.arrive_and_wait();
		Func();
		should_enter_critical_section = false;
		barrier_leave.arrive_and_wait();
	}

	void operator()()
	{
		bool b = false;

		{
			std::unique_lock<std::mutex> lck(mutex);
			b = should_enter_critical_section;
		}

		if(b)
		{
			barrier_enter.arrive_and_wait();
			barrier_leave.arrive_and_wait();
		}
	}
} critical_section;

void exit_callback()
{
	std::quick_exit(1);
}

fox::console::function<void()> cmd_exit("exit", &exit_callback);

fox::console::variable<std::string> test_var("message", "Type 'message abc'");

void thread_function()
{
	using namespace std::chrono_literals;

	while(true)
	{
		std::this_thread::sleep_for(1s);

		// Check if we should enter a critical section
		critical_section();

		std::cout << test_var.get() << '\n';
	}
}

int main()
{
	using namespace std::string_view_literals;

	std::cout.setf(std::ios::unitbuf);

	std::jthread thread(&thread_function);

	while(true)
	{
		std::string cmd;
		std::getline(std::cin, cmd);

		critical_section.trigger([&]()
			{
				const auto results = fox::console::console_manager::instance().execute(cmd);

				for(auto&& r : results)
				{
					if(r.has_value())
					{
						std::cout << r.value().out << '\n';
					}
					else
					{
						std::cout << r.error().message() << '\n';
					}
				}
			}
		);
	}
}