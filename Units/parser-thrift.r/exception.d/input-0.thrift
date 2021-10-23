// Taken from https://freecontent.manning.com/apache-thrift-handling-exceptions/
exception BadFish {
  1: string       fish,       //The problem fish
  2: i16          error_code, //The service specific error code
}
  
service TradeHistory {
  double GetLastSale(1: string fish)
    throws (1: BadFish bf),
}
  
