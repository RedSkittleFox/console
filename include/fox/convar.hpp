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

namespace fox
{
	struct console_invoke_error
	{
		
	};

	enum class console_entity_type
	{
		variable_getter,
		variable_setter,
		command
	};

	namespace details
	{
		class console_base;
	}

	class console_manager
	{
		friend class details::console_base;

		std::unordered_multimap<std::string_view, details::console_base*> commands_;

	private:
		void register_command(std::string_view name, details::console_base* ptr);
		void unregister_command(details::console_base* ptr);
	public:

	};

	namespace details
	{
		class console_base
		{
		public:
			struct signature
			{
				struct parameter
				{
					std::string name;
					std::string type;
				};

				std::string return_type;
				std::vector<parameter> parameters;
			};

		private:
			std::string name_;
			std::string description_;
			std::size_t permission_level_;
			signature signature_;
			console_entity_type type_;
			bool hidden_;

		public:
			[[nodiscard]] const signature& function_signature() const noexcept
			{
				return signature_;
			}

		public:
			console_base() = delete;
			console_base(const console_base&) = delete;
			console_base& operator=(const console_base&) = delete;

			console_base(console_base&&) noexcept = default;
			console_base& operator=(console_base&&) noexcept = default;
			virtual ~console_base() noexcept = 0;

			console_base(std::string_view name, std::string_view description, std::size_t permission_level, signature&& sig, 
				console_entity_type type, bool hidden)
				: name_(static_cast<std::string>(name))
				, description_(static_cast<std::string>(description))
				, permission_level_(permission_level)
				, signature_(std::move(sig))
				, type_(type)
				, hidden_(hidden)
			{}

		public:
			virtual std::expected<std::string, console_invoke_error> invoke(std::span<std::string_view> arguments) = 0;
		};
	}
	
	template<class>
	class concommand;

	template<class R, class... Args>
	class concommand<R(Args...)> : public ::fox::details::console_base
	{
		template<class T> friend class convar;
		
		using arg_definition_t = 
			std::conditional_t<std::is_same_v<R, void>
				std::array<std::string_view, sizeof...(Args)>,
				std::tuple<std::string_view, std::array<std::string_view, sizeof...(Args)>>
			>;

	private:
		template<class Func>
		concommand(
			std::string_view name, 
			Func&& func, 
			std::string_view description = "", 
			std::size_t permission_level = 0,
			bool hidden = false,
			signature sig, 
			console_entity_type type
			)
		{

		}

	public:
		template<class Func>
		concommand(
			std::string_view name, 
			Func&& func, 
			std::string_view description = "", 
			std::size_t permission_level = 0,
			bool hidden = false,
			signature sig = {}
			)
			: concommand(name, std::forward<Func>(func), description, permission_level, hidden, std::move(sig), console_entity_type::command)
		{

		}
	};

	template<class T>
	class convar
	{
		T value_;
		
		concommand<T()> getter_;
		concommand<void(const T&)> setter_;

	public:
		convar(std::string_view name, const T& default_val = T(), std::string_view description = "", std::size_t permission_level = 0)
			: value_(default_val)
			, setter_(name, [this] (const T& v) -> void { return value_ = v; }, description, 0, false, signature{}, console_entity_type::variable_getter )
			, getter_(name, [this] () -> T { return value_; }, description, 0, false, signature{}, console_entity_type::variable_getter )
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


	};

	void console_manager::register_command(std::string_view name, details::console_base* ptr)
	{
		auto [first, last] = commands_.equal_range(name);

		for(const auto& c : std::ranges::subrange(first, last) | std::views::values)
		{
			if(c->function_signature().parameters == ptr->function_signature().parameters) 
				// Same signature registered twice
			{
				throw std::runtime_error("Function already registered.");
			}
		}
			
		commands_.insert(std::make_pair(name, ptr));
	}

	void console_manager::unregister_command(details::console_base* ptr)
	{
		std::erase_if(commands_, [=](const auto& p) -> bool { return p.second == ptr; });
	}
}

#endif