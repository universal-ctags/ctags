int  main( int  argc, char ** argv )
{
	String sample(argc); // local
	List<String> listOfStrings(argv); // local

	foreach(String * p,listOfStrings) // NOT a prototype
	{
		p->dump(); // NOT a prototype
	}

}