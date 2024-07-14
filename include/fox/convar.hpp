#ifndef FOX_CONVAR_CONVAR_H_
#define FOX_CONVAR_CONVAR_H_
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

namespace fox::convar
{
	struct console_invoke_error
	{
		
	};

	class convar_exception : public std::runtime_error
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
		std::expected<std::string, console_invoke_error> execute(std::string_view command, std::span<std::string_view> arguments);

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
			virtual std::expected<std::string, console_invoke_error> invoke(std::span<std::string_view> arguments) = 0;
			[[nodiscard]] const std::string& name() const noexcept { return name_; }
			[[nodiscard]] std::size_t number_of_parameters() const noexcept { return number_of_parameters_; }
			[[nodiscard]] std::string_view description() const noexcept { return description_; }
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

	namespace configuration
	{
		template<class T>
		struct name;

		template<class From>
		struct recast_type;

		template<class T>
		struct parser;
	}

	namespace default_configuration
	{
		template<class T>
		struct name
		{
			[[nodiscard]] std::string operator()() const 
			{
				return typeid(T).name();
			}
		};

		template<class From>
		struct recast_type
		{
			using type = From;
			[[nodiscard]] From operator()(const From& v) const
			{
				return v;
			}
		};

		template<> struct name<std::string> { [[nodiscard]] std::string operator()() const { return "string"; } };

		template<class T>
		struct parser
		{
			[[nodiscard]] std::optional<std::string> operator()(const T& v) const
			{
				static_assert(::fox::convar::details::out_streamable<T>, 
					"Type [T] is does not support ostream operator. Provide custom parser.");

				std::ostringstream ss;
				ss << v;
				return ss.str();
			}

			[[nodiscard]] std::optional<T> operator()(std::string_view sv) const
			{
				static_assert(std::is_default_constructible_v<T>,
					"Type [T] is does not default constructible. Provide custom parser.");

				static_assert(::fox::convar::details::in_streamable<T>,
					"Type [T] is does not support istream operator. Provide custom parser.");

				T v;
				try
				{
					std::istringstream ss(static_cast<std::string>(sv));
					ss >> v;
					return v;
				} catch(...)
				{
					return std::nullopt;
				}
			}
		};

		template<>
		struct parser<void>
		{

		};
	}

	// test

	namespace details_configuration
	{
		template<class T>
		concept custom_parser = requires(const T & v, std::string_view sv)
		{
			{ ::fox::convar::configuration::parser<T>{}(v) } -> std::same_as<std::string>;
			{ ::fox::convar::configuration::parser<T>{}(sv) } -> std::same_as<std::optional<T>>;
		};

		template<class T>
		using parser = std::conditional_t<
			custom_parser<T>,
			::fox::convar::configuration::parser<T>,
			::fox::convar::default_configuration::parser<T>
		>;
	}

	template<class>
	class concommand;

	template<class R, class... Args>
	class concommand<R(Args...)> : public ::fox::convar::details::console_base
	{
		template<class T> friend class convar;

		std::move_only_function<R(Args...)> function_;
		static constexpr std::size_t number_of_parameters = sizeof...(Args);

	private:
		template<class Func>
		concommand(
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
		concommand(
			std::string_view name, 
			Func&& func, 
			std::string_view description = "", 
			std::size_t permission_level = 0,
			bool hidden = false
			)
			: concommand(name, std::forward<Func>(func), description, permission_level, hidden, details::console_entity_type::command)
		{

		}

	public:
		[[nodiscard]] std::expected<std::string, console_invoke_error>
		invoke(std::span<std::string_view> arguments) override
		{
			if(std::size(arguments) != this->number_of_parameters)
			{
				return std::unexpected(console_invoke_error{});
			}

			if constexpr(this->number_of_parameters == 0)
			{
				if constexpr (std::is_void_v<R>)
				{
					function_();
					return "";
				}
				else
				{
					if(std::optional<std::string> v = details_configuration::parser<std::unwrap_ref_decay_t<R>>{}(function_()); v.has_value())
					{
						return v.value();
					}
					else
					{
						return std::unexpected(console_invoke_error{});
					}
				}
			}
			else
			{
				std::array<bool, sizeof...(Args)> all_valid;

				auto parse_type = [&]<std::size_t I>() -> std::tuple_element_t<I, std::tuple<std::unwrap_ref_decay_t<Args>...>>
				{
					using type = std::tuple_element_t<I, std::tuple<std::unwrap_ref_decay_t<Args>...>>;
					using parser = details_configuration::parser<type>;

					std::optional<type> opt = parser{}(arguments[I]);

					all_valid[I] = opt.has_value();

					return opt.value_or(type());
				};

				std::tuple<std::unwrap_ref_decay_t<Args>...> args = [&]<std::size_t... Is>(std::index_sequence<Is...>) -> std::tuple<std::unwrap_ref_decay_t<Args>...>
				{
					return std::make_tuple(parse_type.template operator()<Is>()...);
				}(std::index_sequence_for<Args...>{});

				if (std::reduce(std::begin(all_valid), std::end(all_valid), true, std::logical_and<bool>{}) == false)
				{
					return std::unexpected(console_invoke_error{});
				}

				if constexpr (std::is_void_v<R>)
				{
					std::apply(function_, args);
					return "";
				}
				else
				{
					if (std::optional<std::string> v = details_configuration::parser<R>{}(std::apply(function_, args)); v.has_value())
					{
						return v.value();
					}
					else
					{
						return std::unexpected(console_invoke_error{});
					}
				}
			}
		}
	};

	template<class T>
	class convar
	{
		T value_;
		
		concommand<T()> getter_;
		concommand<T(const T&)> setter_;

	public:
		convar(std::string_view name, const T& default_val = T(), std::string_view description = "", std::size_t permission_level = 0, bool hidden = false)
			: value_(default_val)
			, setter_(name, [this] (const T& v) -> T { return value_ = v; }, description, permission_level, hidden, details::console_entity_type::variable_setter)
			, getter_(name, [this] () -> T { return value_; }, description, permission_level, hidden, details::console_entity_type::variable_getter)
		{}

	public:
		operator T& () noexcept { return value_; }
		operator const T& () const noexcept { return value_; }

	public:
		[[nodiscard]] T& get() noexcept
		{
			return value_;
		}

		[[nodiscard]] const T& get() const noexcept
		{
			return value_;
		}

	public:
		[[nodiscard]] std::string_view description() const noexcept
		{

		}

		[[nodiscard]] std::size_t permission_level() const noexcept
		{
			
		}

		[[nodiscard]] bool hidden() const noexcept
		{
			
		}
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
				throw convar_exception(oss.str());
			}
		}
			
		commands_.insert(std::make_pair(name, ptr));
	}

	inline void console_manager::unregister_command(details::console_base* ptr) noexcept
	{
		std::erase_if(commands_, [=](const auto& p) -> bool { return p.second == ptr; });
	}

	inline std::expected<std::string, console_invoke_error> console_manager::execute(std::string_view command, std::span<std::string_view> arguments)
	{
		const auto r = commands_.equal_range(command);

		for (auto c : std::ranges::subrange(r.first, r.second) | std::views::values)
		{
			if ( c->number_of_parameters() != std::size(arguments) )
				continue;

			return c->invoke(arguments);
		}
		
		return std::unexpected(console_invoke_error());
	}
}

#endif