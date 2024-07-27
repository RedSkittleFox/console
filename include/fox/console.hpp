#ifndef FOX_CONSOLE_CONSOLE_H_
#define FOX_CONSOLE_CONSOLE_H_
#pragma once

#include <string>
#include <string_view>
#include <span>
#include <any>
#include <expected>
#include <vector>
#include <type_traits>
#include <tuple>
#include <utility>
#include <unordered_map>
#include <ranges>
#include <stdexcept>
#include <functional>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <initializer_list>
#include <variant>

namespace fox::console
{
	class register_exception : public std::runtime_error
	{
	public:
		using runtime_error::runtime_error;
	};

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

	struct invoke_error_invalid_permission
	{
		std::size_t expected;
		std::size_t actual;

		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Invalid permission level. Expected '" << expected << "', actual '" << actual << "'.").str();
		}
	};

	struct invoke_error_invalid_number_of_arguments
	{
		std::size_t expected;
		std::size_t actual;

		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Invalid number of arguments. Expected '" << expected << "', actual '" << actual << "'.").str();
		}
	};

	struct invoke_error_return_value_parsing
	{
		std::string from_type;

		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Failed whilst parsing return value from type '" << from_type << "'.").str();
		}
	};

	struct invoke_error_argument_parsing
	{
		std::string from;
		std::string to_type;

		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Failed whilst parsing '" << from << "' to '" << to_type << "'.").str();
		}
	};

	struct invoke_error_invalid_command
	{
		std::string name;
		std::size_t number_of_arguments;

		[[nodiscard]] std::string message() const noexcept
		{
			return (std::ostringstream() << "Unknown command '" << name << "' taking '" << number_of_arguments << "' arguments.").str();
		}
	};

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
		[[nodiscard]] std::string message() const noexcept
		{
			return std::visit([](auto&& cmd) -> std::string { return cmd.message(); }, *this);
		}

		template<class T>
		[[nodiscard]] static auto unexpected(T&& v)
		{
			return std::unexpected(invoke_error(std::in_place_type<T>, std::forward<T>(v)));
		}
	};

	struct invoke_output
	{
		std::string out;
		std::string error;

		invoke_output() = default;
		invoke_output(const invoke_output&) = default;
		invoke_output(invoke_output&&) noexcept = default;
		invoke_output& operator=(const invoke_output&) = default;
		invoke_output& operator=(invoke_output&&) noexcept = default;
		~invoke_output() noexcept = default;

		invoke_output(std::string_view out) : out(static_cast<std::string>(out)) {}
		invoke_output(std::string_view out, std::string_view error) : out(static_cast<std::string>(out)), error(static_cast<std::string>(error)) {}

		operator const std::string& () const noexcept
		{
			return out;
		} 

		auto operator<=>(const invoke_output&) const = default;
	};

	using invoke_result = std::expected<invoke_output, invoke_error>;

	class console_manager
	{
		friend class details::console_base;

		std::unordered_multimap<std::string_view, details::console_base*> commands_;

	public:
		console_manager() = default;

		console_manager(const console_manager&) = delete;
		console_manager(console_manager&&) noexcept = delete;

		console_manager& operator=(const console_manager&) = delete;
		console_manager& operator=(console_manager&&) noexcept = delete;

		~console_manager() noexcept = default;

	private:
		void register_command(std::string_view name, details::console_base* ptr);
		void unregister_command(details::console_base* ptr) noexcept;

	public:
		[[nodiscard]] invoke_result invoke(std::string_view command, std::span<std::string_view> arguments,
			std::size_t permissions = std::numeric_limits<std::size_t>::max());

		[[nodiscard]] invoke_result invoke(std::string_view command, std::initializer_list<std::string_view> arguments,
			std::size_t permissions = std::numeric_limits<std::size_t>::max())
		{
			std::vector args(std::from_range, arguments);
			return this->invoke(command, args, permissions);
		}

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
		[[nodiscard]] std::vector<invoke_result> execute(std::string_view cmd, 
			std::size_t permissions = std::numeric_limits<std::size_t>::max()
		)
		{
			constexpr std::string_view ws = " \t\n\r\f\v";

			std::vector<invoke_result> invoke_results;

			// Used to hold ownership in case of arg modification.
			// We usually use the original string though.
			std::vector<std::string> tmp_args_buffer; 
			std::vector<std::string_view> args;

			std::size_t k = 0;
			std::size_t i = 0;

			auto push_arg = [&]()
				{
					std::string_view sv = cmd.substr(k, i - k);
					if (std::empty(sv))
						return;

					if (std::size(sv) == 1 && ws.find_first_of(sv[0]) != std::string_view::npos)
					{
						return;
					}

					sv.remove_prefix(sv.find_first_not_of(ws));
					if(auto e = sv.find_last_not_of(ws); e != std::string_view::npos)
						sv = sv.substr(0, e + 1);

					if(!std::empty(sv))
						args.push_back(sv);
				};

			bool state_escaped = false;
			for(; i < std::size(cmd); ++i)
			{
				const char c = cmd[i];
				if(state_escaped == false)
				{
					if(c == '"')
					{
						push_arg();
						state_escaped = true;
						k = i + 1;
					}
					else if(c == ';')
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
				else
				{
					if (((i < 2 || cmd[i - 2] == '\\' ) || cmd[i - 1] != '\\') && cmd[i] == '"')
					{
						// Don't use push_arg(), we don't want to trim the string
						std::string& str = tmp_args_buffer.emplace_back(static_cast<std::string>(cmd.substr(k, i - k)));

						// Check if last buffer has escape sequences and if so remove them
						for(std::size_t j = {}; j < std::size(str); ++j)
						{
							if(j + 1 < std::size(str) && str[j] == '\\')
							{
								str[j] = '\0';

								switch (str[j + 1])
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

			push_arg();
			if(!std::empty(args))
			{
				invoke_results.emplace_back(
					this->invoke(args[0], std::span<std::string_view>(std::data(args) + 1, std::size(args) - 1), permissions)
				);

				args.clear();
			}

			return invoke_results;
		}

	public:
		[[nodiscard]] static console_manager& instance();
	};

	namespace details
	{
		class console_base
		{
		private:
			std::string name_;
			std::size_t number_of_parameters_;
			std::string description_;
			std::size_t permission_level_;
			console_entity_type type_;
			bool hidden_;

		public:
			console_base() = delete;
			console_base(const console_base&) = delete;
			console_base& operator=(const console_base&) = delete;

			console_base(console_base&&) noexcept = default;
			console_base& operator=(console_base&&) noexcept = default;

			console_base(std::string_view name, std::size_t number_of_parameters, std::string_view description, std::size_t permission_level, console_entity_type type, bool hidden)
				: name_(static_cast<std::string>(name))
				, number_of_parameters_(number_of_parameters)
				, description_(static_cast<std::string>(description))
				, permission_level_(permission_level)
				, type_(type)
				, hidden_(hidden)
			{
				console_manager::instance().register_command(name_, this);
			}

			virtual ~console_base() noexcept
			{
				console_manager::instance().unregister_command(this);
			}

		public:
			virtual invoke_result invoke(std::span<std::string_view> arguments) = 0;
			[[nodiscard]] const std::string& name() const noexcept { return name_; }
			[[nodiscard]] std::size_t number_of_parameters() const noexcept { return number_of_parameters_; }
			[[nodiscard]] const std::string& description() const noexcept { return description_; }
			[[nodiscard]] std::size_t permission_level() const noexcept { return permission_level_; }
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

	template<class T>
	struct parser;

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

	template<class T>
	concept parsable = ( details::default_parsable<T> || details::custom_parsable<T> );

	template<class>
	class function;

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

	public:
		[[nodiscard]] invoke_result
		invoke(std::span<std::string_view> arguments) override
		{
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

			std::array<bool, sizeof...(Args)> all_valid;
			std::array<const char*, sizeof...(Args)> type_names;

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

			std::tuple<std::unwrap_ref_decay_t<Args>...> args = [&]<std::size_t... Is>(std::index_sequence<Is...>) -> std::tuple<std::unwrap_ref_decay_t<Args>...>
			{
				return std::make_tuple(parse_type.template operator() < Is > ()...);
			}(std::index_sequence_for<Args...>{});

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

			if constexpr (std::is_void_v<R>)
			{
				std::apply(function_, args);
				return invoke_output{};
			}
			else
			{
				if (auto v = details::parser<R>{}(std::apply(function_, args)); v.has_value())
				{
					return v.value();
				}
				else
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
		T value_;
		function<T()> getter_;
		function<T(const T&)> setter_;

	public:
		variable(std::string_view name, const T& default_val = T(), std::string_view description = "", std::size_t permission_level = 0, bool hidden = false)
			: value_(default_val)
			, getter_(name, [this] () -> T { return value_; }, description, permission_level, hidden, details::console_entity_type::variable_getter)
			, setter_(name, [this] (const T& v) -> T { return value_ = v; }, description, permission_level, hidden, details::console_entity_type::variable_setter)
		{}

	public:
		operator T& () noexcept { return value_; }
		operator const T& () const noexcept { return value_; }

	public:
		[[nodiscard]] auto get() noexcept -> T& { return value_; }
		[[nodiscard]] auto get() const noexcept -> const T& { return value_; }
		[[nodiscard]] auto name() const noexcept -> const std::string& { return getter_.name(); }
		[[nodiscard]] auto description() const noexcept -> const std::string& { return getter_.description(); }
		[[nodiscard]] auto permission_level() const noexcept -> std::size_t { return getter_.permission_level(); }
		[[nodiscard]] auto hidden() const noexcept -> bool { return getter_.hidden(); }
		[[nodiscard]] auto getter() noexcept -> decltype(auto) { return getter_; }
		[[nodiscard]] auto setter() noexcept -> decltype(auto) { return setter_; }
		[[nodiscard]] auto getter() const noexcept -> decltype(auto) { return getter_; }
		[[nodiscard]] auto setter() const noexcept -> decltype(auto) { return setter_; }
	};

	inline void console_manager::register_command(std::string_view name, details::console_base* ptr)
	{
		auto [first, last] = commands_.equal_range(name);

		for(const auto& c : std::ranges::subrange(first, last) | std::views::values)
		{
			if(c->number_of_parameters() == ptr->number_of_parameters())
				// Same signature registered twice
			{
				std::ostringstream oss;
				oss << "Function '" << c->name() << "' taking '" << c->number_of_parameters() << "' already exists.";
				throw register_exception(oss.str());
			}
		}
			
		commands_.insert(std::make_pair(name, ptr));
	}

	inline void console_manager::unregister_command(details::console_base* ptr) noexcept
	{
		std::erase_if(commands_, [=](const auto& p) -> bool { return p.second == ptr; });
	}

	inline invoke_result
	console_manager::invoke(std::string_view command, std::span<std::string_view> arguments, std::size_t permissions)
	{
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

			return c->invoke(arguments);
		}
		
		return std::unexpected(invoke_error_invalid_command
			{
				.name = static_cast<std::string>(command),
				.number_of_arguments = std::size(arguments)
			}
		);
	}
}

#endif