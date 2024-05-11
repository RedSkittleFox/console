#include <fox/convar.hpp>

fox::convar<int> test_var("test", 1);

fox::concommand<int()> test_func("test", []() -> int { return 1; } );

int main()
{

}