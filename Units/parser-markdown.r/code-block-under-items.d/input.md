<!-- Taken from #3625 submitted by @jiz4oh -->
# test

- primary key：

  ```sql
  # method 1
  create table department2(
      id int primary key,
      name varchar(20),
      comment varchar(100)
      );
  
  # method 2
  create table department3(
      id int,
      name varchar(20),
      comment varchar(100),
      constraint pk_name primary key(id); 
  ```

- second key:

  ```sh
  foo()
  {
	  :
  }
  ```
