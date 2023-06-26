fn main() {
	assert sample_data['int'] or { 0 }.as_map()['0'] or { 0 }.int() == 1
    a := 1
	assert json.encode(StructType[string]{ val: '' }) == '{"val":""}'
	assert json.map_from(StructType[StructType[string]]{StructType[string]{'3'}}).str() == '{"val":{"val":"3"}}'
}
