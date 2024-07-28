#ifndef FOX_CONSOLE_CONSOLE_H_
#define FOX_CONSOLE_CONSOLE_H_
#pragma once

#include <string>
#include <string_view>
#include <span>
#include <expected>
#include <vector>
#include <type_traits>
#include <tuple>
#include <utility>
#include <unordered_map>
#include <ranges>
#include <functional>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <initializer_list>
#include <variant>
#include <cassert>

#ifdef FOX_CONSOLE_ENABLE_EXCEPTIONS
#include <stdexcept>
#endif

#ifdef FOX_CONSOLE_THREAD_SAFE
#include <mutex>

#ifndef FOX_CONSOLE_THREAD_SAFE_MUTEX
#define FOX_CONSOLE_THREAD_SAFE_MUTEX(X) mutable std::mutex X;
#endif

#ifndef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK
#define FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(LOCK, X) std::unique_lock<std::mutex> LOCK(X);
#endif

#ifndef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK
#define FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK(LOCK) LOCK.unlock();
#endif

#else

#ifndef FOX_CONSOLE_THREAD_SAFE_MUTEX
#define FOX_CONSOLE_THREAD_SAFE_MUTEX(X)
#endif

#ifndef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK
#define FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(LOCK, X)
#endif

#ifndef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK
#define FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK(LOCK)
#endif

#endif

namespace fox::console
{

#ifdef FOX_CONSOLE_ENABLE_EXCEPTIONS
	/// @brief Defines a type of object to be thrown as exception.
	/// It reports errors that are a consequence of invalid console::variable or console::function registration.
	class register_exception : public std::runtime_error
	{
	public:
		using runtime_error::runtime_error;
	};
#endif

	/// @cond undocumented
	namespace details
	{
		enum class console_entity_type
		{
			variable_getter,
			variable_setter,
			command
		};

		class console_base;
	}
	/// @endcond 

	/// @brief Defines an error type returned when command is invoked with invalid permissions.
	struct invoke_error_invalid_permission
	{
		/// @brief Permissions required by the command.
		std::size_t expected;
		/// @brief Permissions provided by the caller.
		std::size_t actual;

		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Invalid permission level. Expected '" << expected << "', actual '" << actual << "'.").str();
		}
	};

	/// @brief Defines an error type returned when invalid number of arguments is passed.
	struct invoke_error_invalid_number_of_arguments
	{
		/// @brief Number of arguments required by the command.
		std::size_t expected;
		/// @brief Number of arguments provided by the caller.
		std::size_t actual;

		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Invalid number of arguments. Expected '" << expected << "', actual '" << actual << "'.").str();
		}
	};

	/// @brief Defines an error type returned when parsing of the return value fails.
	struct invoke_error_return_value_parsing
	{
		/// @brief Type of variable which the parsing failed for.
		std::string from_type;

		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Failed whilst parsing return value from type '" << from_type << "'.").str();
		}
	};

	/// @brief Defines an error type returned when parsing of the argument fails.
	struct invoke_error_argument_parsing
	{
		/// @brief String which was being parsed.
		std::string from;

		/// @brief Data type which the string was parsed into.
		std::string to_type;

		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Failed whilst parsing '" << from << "' to '" << to_type << "'.").str();
		}
	};

	/// @brief Defines an error type returned when invalid command is invoked.
	struct invoke_error_invalid_command
	{
		/// @brief Command name.
		std::string name;

		/// @brief Number of arguments used.
		std::size_t number_of_arguments;

		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Unknown command '" << name << "' taking '" << number_of_arguments << "' arguments.").str();
		}
	};

	/// @brief Generalized version of the invoke error.
	/// Use std::holds_alternative together with std::get to extract exact error type.
	struct invoke_error : std::variant<
		invoke_error_invalid_permission,
		invoke_error_invalid_number_of_arguments,
		invoke_error_return_value_parsing,
		invoke_error_argument_parsing,
		invoke_error_invalid_command
	>
	{
		using variant::variant;

	public:
		/// @brief Formatted string message for the error.
		///	@returns Message string
		[[nodiscard]] std::string message() const noexcept
		{
			return std::visit([](auto&& cmd) -> std::string { return cmd.message(); }, *this);
		}

	public:
		/// @cond undocumented
		// Internal helper
		template<class T>
		[[nodiscard]] static auto unexpected(T&& v) // TODO: Hide this
		{
			return std::unexpected(invoke_error(std::in_place_type<T>, std::forward<T>(v)));
		}
		/// @endcond undocumented
	};

	/// @brief Output returned by the callee during correct execution.
	struct invoke_output
	{
		/// @brief Regular output. Equivalent to stdout
		std::string out;
		/// @brief Error output. Equivalent to stderr.
		std::string error;

		/// @brief Compares lhs and rhs lexicographically by synthetic three-way comparison.
		auto operator<=>(const invoke_output&) const = default;
	};

	/// @brief std::expected result type returned by the function invocation. 
	using invoke_result = std::expected<invoke_output, invoke_error>;

	/// @brief Singleton responsible for managing console::function and console::variable tracking and invoking.
	///	@note Library user is responsible for implementing the ``console_manager::instance()`` handler somewhere in their codebase.
	class console_manager
	{
		friend class details::console_base;

		FOX_CONSOLE_THREAD_SAFE_MUTEX(mutex_)
		std::unordered_multimap<std::string_view, details::console_base*> commands_;

	public:
		/// @brief Default constructor.
		console_manager() = default;

		/// @brief Copy constructor is deleted.
		console_manager(const console_manager&) = delete;

		/// @brief Move constructor is deleted.
		console_manager(console_manager&&) noexcept = delete;

		/// @brief Copy assignment operator is deleted.
		///	@returns *this
		console_manager& operator=(const console_manager&) = delete;

		/// @brief Move assignment operator is deleted.
		///	@returns *this
		console_manager& operator=(console_manager&&) noexcept = delete;

		/// @brief Destructor.
		~console_manager() noexcept = default;

	private:
		// These two are not thread safe, lock mutex_ externally, done for consistency's sake
		void register_command(std::string_view name, details::console_base* ptr);
		void unregister_command(details::console_base* ptr) noexcept;

	public:
		/// @brief Invokes the specified command with the set of parameters. Returns expected value `invoke_output` if call was successful, unexpected value `invoke_error` otherwise.
		///	@param command	Command to be invoked
		///	@param arguments Arguments to be used during invocation
		///	@param permissions Permission level used to invoke the command
		///	@returns `std::expected<invoke_output, invoke_error>`
		[[nodiscard]] invoke_result invoke(std::string_view command, std::span<std::string_view> arguments,
			std::size_t permissions = std::numeric_limits<std::size_t>::max());

		/// @brief Invokes the specified command with the set of parameters. Returns expected value `invoke_output` if call was successful, unexpected value `invoke_error` otherwise.
		///	@param command	Command to be invoked
		///	@param arguments Arguments to be used during invocation
		///	@param permissions Permission level used to invoke the command
		///	@returns `std::expected<invoke_output, invoke_error>`
		[[nodiscard]] invoke_result invoke(std::string_view command, std::initializer_list<std::string_view> arguments,
			std::size_t permissions = std::numeric_limits<std::size_t>::max())
		{
			std::vector args(std::from_range, arguments);
			return this->invoke(command, args, permissions);
		}

		/// @brief Invokes the specified command with the set of parameters. Returns expected value `invoke_output` if call was successful, unexpected value `invoke_error` otherwise.
		/// @tparam Range Type of the arguments range
		///	@param command	Command to be invoked
		///	@param r Arguments to be used during invocation
		///	@param permissions Permission level used to invoke the command
		///	@returns `std::expected<invoke_output, invoke_error>`
		template<class Range>
		[[nodiscard]] invoke_result invoke(std::string_view command, const Range& r, 
			std::size_t permissions = std::numeric_limits<std::size_t>::max())
			requires (
				!std::convertible_to<Range, std::span<std::string_view>> &&
				std::convertible_to<std::ranges::range_value_t<const Range&>, std::string_view>
			)
		{
			std::vector<std::string_view> args;
			args.reserve(std::size(r));
			for (auto&& e : r)
				args.emplace_back(e);

			return this->invoke(command, static_cast<std::span<std::string_view>>(args), permissions);
		}

	public:
		/// @brief Command line input parser.
		/// @details Multiple commands can be chained together using `;` character.
		/// Quotation marks "" can be used to pass multiword arguments.
		///	@param cmd Command string to be parsed.
		///	@param permissions Permission level used to invoke the command
		///	@returns std::vector of `std::expected<invoke_output, invoke_error>` of resulting function calls.
		[[nodiscard]] std::vector<invoke_result> execute(std::string_view cmd, 
			std::size_t permissions = std::numeric_limits<std::size_t>::max()
		);

	public:
		/// @brief fox::console::console_manager singleton instance.
		///	@note This function has to be implemented by the library user.
		///	@returns Refernece to global command manager instance.
		[[nodiscard]] static console_manager& instance();
	};

	/// @cond undocumented
	namespace details
	{
		class console_base
		{
			// Access invoke
			friend class ::fox::console::console_manager;

		private:
			const std::string name_;
			const std::size_t number_of_parameters_;
			const std::string description_;
			const std::size_t permission_level_;
			const console_entity_type type_;
			const bool hidden_;

			FOX_CONSOLE_THREAD_SAFE_MUTEX(mutex_)

		public:
			console_base() = delete;
			console_base(const console_base&) = delete;
			console_base& operator=(const console_base&) = delete;

			console_base(console_base&&) noexcept = delete;
			console_base& operator=(console_base&&) noexcept = delete;

			console_base(std::string_view name, std::size_t number_of_parameters, std::string_view description, std::size_t permission_level, console_entity_type type, bool hidden)
				: name_(static_cast<std::string>(name))
				, number_of_parameters_(number_of_parameters)
				, description_(static_cast<std::string>(description))
				, permission_level_(permission_level)
				, type_(type)
				, hidden_(hidden)
			{
				FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, console_manager::instance().mutex_)
				console_manager::instance().register_command(name_, this);
			}

			virtual ~console_base() noexcept
			{
				// We need to first lock the mutex of the console manager in order to avoid possible recursive lock
				FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock1, console_manager::instance().mutex_)
				FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock2, mutex_) // Stall destructor if object is being called
				console_manager::instance().unregister_command(this);
			}

		protected:
			virtual invoke_result invoke(std::span<std::string_view> arguments) = 0;

		public:
			/// @brief Get function name.
			///	@returns Function name.
			[[nodiscard]] const std::string& name() const noexcept { return name_; }

			/// @brief Get number of parameters.
			///	@returns Number of parameters.
			[[nodiscard]] std::size_t number_of_parameters() const noexcept { return number_of_parameters_; }

			/// @brief Get description.
			///	@returns Description.
			[[nodiscard]] const std::string& description() const noexcept { return description_; }

			/// @brief Get minimal permission level.
			///	@returns Permission level.
			[[nodiscard]] std::size_t permission_level() const noexcept { return permission_level_; }

			/// @brief Check if function's should be hidden.
			///	@returns True if should be hidden, false otherwise.
			[[nodiscard]] bool hidden() const noexcept { return hidden_; }
		};

		template<class T>
		concept out_streamable = requires(std::ostream & os, const T & v)
		{
			{ os << v } -> std::same_as<std::ostream&>;
		};

		template<class T>
		concept in_streamable = requires(std::istream & is, T & v)
		{
			{ is >> v } -> std::same_as<std::istream&>;
		};
	}
	/// @endcond undocumented

	/// @brief User defined argument parser.
	///	@tparam T Parsed type.
	template<class T> struct parser;

	/// @cond undocumented
	namespace details
	{
		template<class T>
		concept streamable = requires(std::ostream & os, const T & out)
		{
			{ os << out } -> std::convertible_to<std::ostream&>;
		} && requires(std::istream& is, T& in)
		{
			{ is >> in } -> std::convertible_to<std::istream&>;
		};

		template<streamable T>
		struct default_parser
		{
			[[nodiscard]] std::optional<invoke_output> operator()(const T& v) const
			{
				static_assert(::fox::console::details::out_streamable<T>,
					"Type [T] is does not support ostream operator. Provide custom parser.");

				std::ostringstream ss;
				ss << v;
				return invoke_output{ ss.str() };
			}

			[[nodiscard]] std::optional<T> operator()(std::string_view sv) const
			{
				static_assert(std::is_default_constructible_v<T>,
					"Type [T] is does not default constructible. Provide custom parser.");

				static_assert(::fox::console::details::in_streamable<T>,
					"Type [T] is does not support istream operator. Provide custom parser.");

				T v;
				try
				{
					std::istringstream ss(static_cast<std::string>(sv));
					ss >> v;
					return v;
				}
				catch (...)
				{
					return std::nullopt;
				}
			}
		};

		template<>
		struct default_parser<std::string>
		{
			[[nodiscard]] std::optional<invoke_output> operator()(const std::string& v) const
			{
				return invoke_output{ v };
			}

			[[nodiscard]] std::optional<std::string> operator()(std::string_view sv) const
			{
				return static_cast<std::string>(sv);
			}
		};

		template<class T>
		concept custom_parsable = requires(const T & t, std::string_view sv)
		{
			{ ::fox::console::parser<std::remove_cvref_t<T>>()(t) } -> std::same_as<std::optional<invoke_output>>;
			{ ::fox::console::parser<std::remove_cvref_t<T>>()(sv) } -> std::same_as<std::optional<std::remove_cvref_t<T>>>;
		};


		template<class T>
		concept default_parsable = 
				(streamable<std::remove_cvref_t<T>>) &&
				requires(const std::remove_cvref_t<T> & t, std::string_view sv)
		{
			{ ::fox::console::details::default_parser<std::remove_cvref_t<T>>().operator()(t) } -> std::same_as<std::optional<invoke_output>>;
			{ ::fox::console::details::default_parser<std::remove_cvref_t<T>>().operator()(sv) } -> std::same_as<std::optional<std::remove_cvref_t<T>>>;
		};

		template<class T, bool B = custom_parsable<T>>
		struct conditional_evaluated_parser 
		{
			using type = ::fox::console::parser<T>;
		};

		template<class T>
		struct conditional_evaluated_parser<T, false>
		{
			using type = ::fox::console::details::default_parser<T>;
		};

		template<class T>
		using parser = typename conditional_evaluated_parser<T>::type;
	}
	/// @endcond undocumented

	/// @brief Specifies that a type is parsable - works with `fox::console::variable` and `fox::console::function`.
	///	@tparam T Parsable type
	template<class T>
	concept parsable = ( details::default_parsable<T> || details::custom_parsable<T> );

	/// @brief /* undefined */
	template<class> class function;

	/// @brief Class template `fox::console::function` is a general purpose polymorphic console function registration proxy.
	/// @details Instances of `fox::console::function` can store and be centrally (via console_manager) invoked.
	///	@tparam R Function's return type
	///	@tparam Args Function's parameter types.
	template<class R, class... Args>
	requires (
		(std::is_void_v<R> || parsable<R>) &&
		(
			sizeof...(Args) == 0 ||
			(sizeof...(Args) == 1 && (std::is_void_v<Args> && ...)) ||
			(parsable<Args> && ...)
			)
	)
	class function<R(Args...)> : public ::fox::console::details::console_base
	{
		template<parsable T> friend class variable;

		std::move_only_function<R(Args...)> function_;
		static constexpr std::size_t number_of_parameters = sizeof...(Args);

	private:
		template<class Func>
		function(
			std::string_view name, 
			Func&& func, 
			std::string_view description, 
			std::size_t permission_level,
			bool hidden,
			details::console_entity_type type
			)
		: console_base(name, number_of_parameters, description, permission_level, type, hidden)
		, function_(std::forward<Func>(func))
		{
			
		}

	public:
		/// @brief Default constructor is deleted.
		function() = delete;

	public:
		/// @brief Constructor that registers function within `fox::console::console_manager` with appropriate parameters.
		///	@param name Function's name in the console.
		///	@param func Callback functor.
		///	@param description Function's description.
		///	@param permission_level Minimum permission level that the function can be invoked with.
		///	@param hidden Is function hidden from the search. Intended for GUI implementations.
		///	@tparam Func Functor's type.
		///	@exception register_exception Thrown if function is already registered.
		template<class Func>
		function(
			std::string_view name, 
			Func&& func, 
			std::string_view description = "", 
			std::size_t permission_level = 0,
			bool hidden = false
			)
		requires std::is_invocable_r_v<R, Func, Args...>
			: function(name, std::forward<Func>(func), description, permission_level, hidden, details::console_entity_type::command)
		{

		}

	protected:
		[[nodiscard]] invoke_result
		invoke(std::span<std::string_view> arguments) override
		{
			// Validate the number of arguments
			if(std::size(arguments) != this->number_of_parameters)
			{
				return std::unexpected(
					invoke_error_invalid_number_of_arguments
					{
						.expected = this->number_of_parameters,
						.actual = std::size(arguments)
					}
				);
			}

			// Helpers used to track if all arguments were parsed properly during parameter pack expansion.
			std::array<bool, sizeof...(Args)> all_valid;
			std::array<const char*, sizeof...(Args)> type_names;

			// Helper used for parameter pack expansion.
			auto parse_type = [&]<std::size_t I>() -> std::tuple_element_t<I, std::tuple<std::unwrap_ref_decay_t<Args>...>>
			{
				using type = std::tuple_element_t<I, std::tuple<std::unwrap_ref_decay_t<Args>...>>;
				using parser = details::parser<type>;

				std::optional<type> opt = parser{}(arguments[I]);

				all_valid[I] = opt.has_value();

				if (all_valid[I] == false)
					type_names[I] = typeid(type).name();

				return opt.value_or(type());
			};

			// Iterate over all argument types by index.
			std::tuple<std::unwrap_ref_decay_t<Args>...> args = [&]<std::size_t... Is>(std::index_sequence<Is...>) -> std::tuple<std::unwrap_ref_decay_t<Args>...>
			{
				return std::make_tuple(parse_type.template operator() < Is > ()...);
			}(std::index_sequence_for<Args...>{});

			// Return an error if argument parsing failed.
			for (auto&& [valid, string, type] : std::views::zip(all_valid, arguments, type_names))
			{
				if (valid) [[likely]]
					continue;

					return invoke_error::unexpected(
						invoke_error_argument_parsing
						{
							.from = static_cast<std::string>(string),
							.to_type = type
						}
					);
			}

			// If return type is void then don't parse it
			if constexpr (std::is_void_v<R>)
			{
				std::apply(function_, args);
				return invoke_output{};
			}
			else // Parse non-void arguments
			{
				if (auto v = details::parser<R>{}(std::apply(function_, args)); v.has_value())
				{
					return v.value();
				}
				else // If parsing failed, return an error.
				{
					return invoke_error::unexpected(
						invoke_error_return_value_parsing
						{
							.from_type = typeid(std::unwrap_ref_decay_t<R>).name()
						}
					);
				}
			}
		}
	};


	template<parsable T>
	class variable
	{
		// Variables are implemented by providing two overloads of the function with the name of the variable.
		// One overload sets the value, another one returns it.
		// Both overloads return the variable's values after their respective operations are complete.
		FOX_CONSOLE_THREAD_SAFE_MUTEX(mutex_)
		T value_; // This one should be destroyed last 
		function<T()> getter_;
		function<T(const T&)> setter_;

	public:
		/// @brief Constructor that registers variable within `fox::console::console_manager` with appropriate parameters.
		/// @param name Variable's name.
		/// @param default_val The value the variable should be initialized with.
		///	@param description Variable's description.
		///	@param permission_level Minimum permission level that the variable can be modified with.
		///	@param hidden Is variable hidden from the search. Intended for GUI implementations.
		///	@exception register_exception Thrown if variable is already registered.
		variable(std::string_view name, const T& default_val = T(), std::string_view description = "", std::size_t permission_level = 0, bool hidden = false)
			: value_(default_val)
			, getter_(name, [this] () -> T { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) return value_; }, description, static_cast<std::size_t>(0), hidden, details::console_entity_type::variable_getter)
			, setter_(name, [this] (const T& v) -> T { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) return value_ = v; }, description, permission_level, hidden, details::console_entity_type::variable_setter)
		{}


	public:
#ifndef FOX_CONSOLE_THREAD_SAFE
		/// @brief Variable type conversion operator.
		///	@returns Reference to managed variable.
		operator T& () noexcept { return value_; }

		/// @brief Variable type conversion operator.
		///	@returns Value of the managed variable.
		operator const T& () const noexcept { return value_; }

		/// @brief Variable value getter.
		///	@returns Reference to the managed variable.
		[[nodiscard]] auto get() noexcept -> T& { return value_; }

		/// @brief Variable value getter.
		///	@returns Value of the managed variable.
		[[nodiscard]] auto get() const noexcept -> const T& { return value_; }

#else
		/// @brief Variable type conversion operator.
		///	@returns Value of the managed variable.
		operator T () const noexcept { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) return value_; }

		/// @brief Variable value getter.
		///	@returns Value of the managed variable.
		[[nodiscard]] auto get() const noexcept -> T { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) return value_; }
#endif

		/// @brief Variable value setter.
		///	@param v New value of the variable.
		auto set(const T& v) const noexcept -> void { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) value_ = v; }

		/// @brief Variable value setter.
		///	@param v New value of the variable.
		auto set(T&& v) const noexcept -> void { FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock, mutex_) value_ = std::move(v); }

		/// @brief Get variable's name.
		///	@returns Function name.
		[[nodiscard]] auto name() const noexcept -> const std::string& { return getter_.name(); }

		/// @brief Get description.
		///	@returns Description.
		[[nodiscard]] auto description() const noexcept -> const std::string& { return getter_.description(); }

		/// @brief Get minimal permission level.
		///	@returns Permission level.
		[[nodiscard]] auto permission_level() const noexcept -> std::size_t { return setter_.permission_level(); }

		/// @brief Check if variable should be hidden.
		///	@returns True if should be hidden, false otherwise.
		[[nodiscard]] auto hidden() const noexcept -> bool { return getter_.hidden(); }

		/// @brief Get underlying getter function.
		///	@returns Reference to getter function.
		[[nodiscard]] auto getter() noexcept -> decltype(auto) { return getter_; }

		/// @brief Get underlying setter function.
		///	@returns Reference to setter function.
		[[nodiscard]] auto setter() noexcept -> decltype(auto) { return setter_; }

		/// @brief Get underlying getter function.
		///	@returns Value of the getter function.
		[[nodiscard]] auto getter() const noexcept -> decltype(auto) { return getter_; }

		/// @brief Get underlying setter function.
		///	@returns Value of the setter function.
		[[nodiscard]] auto setter() const noexcept -> decltype(auto) { return setter_; }
	};

	/// @cond undocumented
	inline void console_manager::register_command(std::string_view name, details::console_base* ptr)
	{
		// Mutex locked by the caller!

		auto [first, last] = commands_.equal_range(name);

		for(const auto& c : std::ranges::subrange(first, last) | std::views::values)
		{
			if(c->number_of_parameters() == ptr->number_of_parameters())
				// Same signature registered twice
			{
#ifdef FOX_CONSOLE_ENABLE_EXCEPTIONS
				std::ostringstream oss;
				oss << "Function '" << c->name() << "' taking '" << c->number_of_parameters() << "' already exists.";
				throw register_exception(oss.str());
#else
				assert(!"Tried to register already existing command. Terminating...");
				std::abort();
#endif
			}
		}
			
		commands_.insert(std::make_pair(name, ptr));
	}

	inline void console_manager::unregister_command(details::console_base* ptr) noexcept
	{
		// Mutex locked by the caller!
		std::erase_if(commands_, [=](const auto& p) -> bool { return p.second == ptr; });
	}

	inline invoke_result
	console_manager::invoke(std::string_view command, std::span<std::string_view> arguments, std::size_t permissions)
	{
		FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock1, mutex_) // Lock console manager which is managing the queue first

		const auto r = commands_.equal_range(command);

		for (auto c : std::ranges::subrange(r.first, r.second) | std::views::values)
		{
			if ( c->number_of_parameters() != std::size(arguments) )
				continue;

			// Invalid permission level
			if (c->permission_level() > permissions)
				return invoke_error::unexpected(
					invoke_error_invalid_permission
					{
						.expected = c->permission_level(),
						.actual = permissions
					}
				);

			// Lock the function's mutex (needed to block the destructor from deleting the object whilst it's being called)
			FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK(lock2, c->mutex_);

			// Unlock the lock of console manager
			FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK(lock1)

			return c->invoke(arguments);
		}
		
		return std::unexpected(invoke_error_invalid_command
			{
				.name = static_cast<std::string>(command),
				.number_of_arguments = std::size(arguments)
			}
		);
	}

	inline std::vector<invoke_result>
	console_manager::execute(std::string_view cmd,
		std::size_t permissions)
	{
		// White space characters
		constexpr std::string_view ws = " \t\n\r\f\v";

		std::vector<invoke_result> invoke_results;

		// Used to hold ownership in case of arg modification.
		// We usually use the original string though.
		std::vector<std::string> tmp_args_buffer;
		std::vector<std::string_view> args;

		std::size_t k = 0;
		std::size_t i = 0;

		// Tries to push currently parsed string into vector
		auto push_arg = [&]()
			{
				std::string_view sv = cmd.substr(k, i - k);

				// Trim the string
				if (auto e = sv.find_first_not_of(ws); e != std::string_view::npos)
					sv.remove_prefix(e);
				if (auto e = sv.find_last_not_of(ws); e != std::string_view::npos)
					sv = sv.substr(0, e + 1);

				// Don't insert empty (after the trim)
				if (!std::empty(sv))
					args.push_back(sv);
			};

		bool state_escaped = false;
		for (; i < std::size(cmd); ++i)
		{
			const char c = cmd[i];
			if (state_escaped == false) // Parse if outside of quotation marks
			{
				if (c == '"')
				{
					push_arg();
					state_escaped = true;
					k = i + 1;
				}
				else if (c == ';')
				{
					push_arg();
					k = i + 1;

					if (!std::empty(args))
					{
						invoke_results.emplace_back(
							this->invoke(args[0], std::span<std::string_view>(std::data(args) + 1, std::size(args) - 1), permissions)
						);

						args.clear();
						tmp_args_buffer.clear();
					}
				}
				else if (ws.find_first_of(c) != std::string_view::npos)
				{
					push_arg();
					k = i + 1;
				}
			}
			else // inside quotation marks
			{
				// Leave only if \", make sure \ itself isn't escaped - \\"
				if (((i < 2 || cmd[i - 2] == '\\') || cmd[i - 1] != '\\') && cmd[i] == '"')
				{
					// Don't use push_arg(), we don't want to trim the string
					std::string& str = tmp_args_buffer.emplace_back(static_cast<std::string>(cmd.substr(k, i - k)));

					// Check if last buffer has escape sequences and if so remove them
					for (std::size_t j = {}; j < std::size(str); ++j)
					{
						if (j + 1 < std::size(str) && str[j] == '\\')
						{
							str[j] = '\0'; // Delete these later

							switch (str[j + 1]) // 
							{
							case '\n':
								str[j + 1] = '\n';
								break;
							case '\t':
								str[j + 1] = '\t';
								break;
							default:
								break;
							}

							j = j + 1;
						}
					}

					std::erase(str, '\0');

					args.push_back(str);

					state_escaped = false;
					k = i + 1;
				}

			}
		}

		// Try to push remaining arg onto the stack and invoke the command if any left
		push_arg();
		if (!std::empty(args))
		{
			invoke_results.emplace_back(
				this->invoke(args[0], std::span<std::string_view>(std::data(args) + 1, std::size(args) - 1), permissions)
			);

			args.clear();
		}

		return invoke_results;
	}
	/// @endcond undocumented
}

#ifdef FOX_CONSOLE_THREAD_SAFE

#ifdef FOX_CONSOLE_THREAD_SAFE_MUTEX
#undef FOX_CONSOLE_THREAD_SAFE_MUTEX
#endif

#ifdef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK
#undef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK
#endif

#ifndef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK
#undef FOX_CONSOLE_THREAD_SAFE_UNIQUE_LOCK_UNLOCK
#endif

#endif

#endif