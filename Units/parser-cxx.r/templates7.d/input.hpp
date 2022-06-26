template<typename A, typename B>
A foo()
{
    return A();
}

template<typename A, typename B>
A& bar(A& a, const B& b)
{
    return a;
}