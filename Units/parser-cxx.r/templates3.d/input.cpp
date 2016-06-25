/* bug reported by akrzyz on 2016.03.10: tags for makeArrayRef were not emitted */

template <typename ValueType, int N, typename SizeType>
typename boost::disable_if_c<N == 1, ArrayRef<ValueType, SizeType> >::type
makeArrayRef(ValueType (&p_data)[N], SizeType& p_size)
{
    return ArrayRef<ValueType, SizeType>(p_data, p_size);
}

void foo()
{
}

