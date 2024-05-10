#ifndef FOX_LEXER_LEXER_H_
#define FOX_LEXER_LEXER_H_
#pragma once

#include <string>
#include <string_view>
#include <span>
#include <any>
#include <expected>

namespace fox
{
	struct console_invoke_error
	{
		
	};

	namespace details
	{
		class console_base
		{
			std::string name_;
			std::string description_;
			std::any user_data_;

		public:
			virtual ~console_base() noexcept = 0;
			virtual std::expected<std::string, console_invoke_error> invoke(std::span<std::string_view> arguments) = 0;
		};
	}

	class console_manager
	{
		
	};

	template<class T>
	class convar : public ::fox::details::console_base
	{
		T value_;

	public:
		convar()
		{
			
		}

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

	template<class>
	class concommand;

	template<class R, class... Args>
	class concommand<R(Args...)> : public ::fox::details::console_base
	{
		
	};
}

#endif